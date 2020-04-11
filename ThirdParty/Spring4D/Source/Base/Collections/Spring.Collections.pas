{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2020 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

/// <summary>
///   The Spring.Collections namespaces introduce the Collections Framework in
///   spring4d.
/// </summary>
unit Spring.Collections;

interface

uses
  Classes,
  Generics.Defaults,
  SysUtils,
  TypInfo,
  Spring;

const
  caAdded = Spring.caAdded;
  caRemoved = Spring.caRemoved;
  caExtracted = Spring.caExtracted;
  caReplaced = Spring.caReplaced;
  caMoved = Spring.caMoved;
  caReset = Spring.caReset;
  caChanged = Spring.caChanged;
  caReseted = Spring.caReset deprecated 'Use caReset instead';

type
  TDictionaryOwnerships = set of (doOwnsKeys, doOwnsValues);

  TCollectionChangedAction = Spring.TCollectionChangedAction;

  TCollectionChangedEvent<T> = procedure(Sender: TObject; const Item: T;
    Action: TCollectionChangedAction) of object;

  TPair<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
    constructor Create(const key: TKey; const value: TValue);
  end;

  ICollectionChangedEvent<T> = interface(IEvent<TCollectionChangedEvent<T>>)
  end;

  INotifyCollectionChanged<T> = interface
    ['{B4F756F2-B436-462D-8046-AB70377228F1}']
  {$REGION 'Property Accessors'}
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  TEqualsMethod<T> = function(const left, right: T): Boolean of object;

  /// <summary>
  ///   Supports a simple iteration over a non-generic collection.
  /// </summary>
  IEnumerator = interface(IInvokable)
    ['{A2AD52DC-FA9F-4121-9B54-5C427DA5E62C}']
  {$REGION 'Property Accessors'}
    function GetCurrent: Spring.TValue;
  {$ENDREGION}

    /// <summary>
    ///   Advances the enumerator to the next element of the collection.
    /// </summary>
    /// <returns>
    ///   <b>True</b> if the enumerator was successfully advanced to the next
    ///   element; <b>False</b> if the enumerator has passed the end of the
    ///   collection.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The collection was modified after the enumerator was created.
    /// </exception>
    function MoveNext: Boolean;

    /// <summary>
    ///   Gets the current element in the collection.
    /// </summary>
    /// <value>
    ///   The current element in the collection.
    /// </value>
    property Current: Spring.TValue read GetCurrent;
  end;

  /// <summary>
  ///   Supports a simple iteration over a generic collection.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of objects to enumerate.
  /// </typeparam>
  IEnumerator<T> = interface
    ['{E6525A22-15EF-46EB-8A68-8CB202DA7D67}']
  {$REGION 'Property Accessors'}
    function GetCurrent: T;
  {$ENDREGION}

    /// <summary>
    ///   Advances the enumerator to the next element of the collection.
    /// </summary>
    /// <returns>
    ///   <b>True</b> if the enumerator was successfully advanced to the next
    ///   element; <b>False</b> if the enumerator has passed the end of the
    ///   collection.
    /// </returns>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The collection was modified after the enumerator was created.
    /// </exception>
    function MoveNext: Boolean;

    /// <summary>
    ///   Gets the current element in the collection.
    /// </summary>
    /// <value>
    ///   The current element in the collection.
    /// </value>
    property Current: T read GetCurrent;
  end;

  /// <summary>
  ///   Exposes an enumerator, which supports a simple iteration over a
  ///   non-generic collection.
  /// </summary>
  IEnumerable = interface(IInvokable)
    ['{6BC97F33-C0A8-4770-8E1C-C2017527B7E7}']
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
    function GetIsEmpty: Boolean;
  {$ENDREGION}

    /// <summary>
    ///   Returns the reference to this instance.
    /// </summary>
    /// <returns>
    ///   The <see cref="TObject" /> instance behind this IEnumerable
    ///   reference.
    /// </returns>
    function AsObject: TObject;

    /// <summary>
    ///   Returns an enumerator that iterates through a collection.
    /// </summary>
    /// <returns>
    ///   An <see cref="IEnumerator" /> object that can be used to iterate
    ///   through the collection.
    /// </returns>
    function GetEnumerator: IEnumerator;

    /// <summary>
    ///   Returns the number of elements in a sequence.
    /// </summary>
    /// <value>
    ///   The number of elements in the sequence.
    /// </value>
    property Count: Integer read GetCount;

    /// <summary>
    ///   Returns the type of the elements in the sequence.
    /// </summary>
    /// <value>
    ///   The type of the elements in the sequence.
    /// </value>
    property ElementType: PTypeInfo read GetElementType;

    /// <summary>
    ///   Determines whether the sequence contains no elements.
    /// </summary>
    /// <value>
    ///   <b>True</b> if the source sequence contains no elements; otherwise, <b>
    ///   False</b>.
    /// </value>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  /// <summary>
  ///   Non generic interface for adding items to a collection - this can be
  ///   used for serialization.
  /// </summary>
  ICollection = interface(IEnumerable)
    ['{4E749779-0873-498E-9597-FCF2A42C3F7B}']

    /// <summary>
    ///   Adds an item to the collection.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the collection
    /// </param>
    /// <returns>
    ///   True if the collection was modified, False otherwise.
    /// </returns>
    function Add(const item: TValue): Boolean;

    /// <summary>
    ///   Removes all items from the collection.
    /// </summary>
    procedure Clear;
  end;

  /// <summary>
  ///   Exposes the enumerator, which supports a simple iteration over a
  ///   collection of a specified type.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of objects to enumerate.
  /// </typeparam>
  /// <seealso href="http://msdn.microsoft.com/en-us/magazine/cc700332.aspx">
  ///   The LINQ Enumerable Class
  /// </seealso>
  IEnumerable<T> = interface
    ['{A6B46D30-5B0F-495F-B7EC-46FBC5A75D24}']
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
    function GetIsEmpty: Boolean;
  {$ENDREGION}

    /// <summary>
    ///   Returns the reference to this instance.
    /// </summary>
    /// <returns>
    ///   The <see cref="TObject" /> instance behind this IEnumerable
    ///   reference.
    /// </returns>
    function AsObject: TObject;

    /// <summary>
    ///   Returns an enumerator that iterates through the collection.
    /// </summary>
    /// <returns>
    ///   An <see cref="IEnumerator&lt;T&gt;" /> that can be used to iterate
    ///   through the collection.
    /// </returns>
    function GetEnumerator: IEnumerator<T>;

    /// <summary>
    ///   Returns the specified comparer for this instance.
    /// </summary>
    /// <returns>
    ///   Returns the specified IComparer&lt;T&gt; for this instance.
    /// </returns>
    function GetComparer: IComparer<T>;

    /// <summary>
    ///   Applies an accumulator function over a sequence.
    /// </summary>
    function Aggregate(const func: Func<T, T, T>): T;

    /// <summary>
    ///   Determines whether all elements of a sequence satisfy a condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <b>True</b> if every element of the source sequence passes the test
    ///   in the specified predicate, or if the sequence is empty; otherwise, <b>
    ///   False</b>.
    /// </returns>
    function All(const predicate: Predicate<T>): Boolean;

    /// <summary>
    ///   Determines whether a sequence contains any elements.
    /// </summary>
    /// <returns>
    ///   <b>True</b> if the source sequence contains any elements; otherwise, <b>
    ///   False</b>.
    /// </returns>
    function Any: Boolean; overload;

    /// <summary>
    ///   Determines whether any element of a sequence satisfies a condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <b>True</b> if any elements in the source sequence pass the test in
    ///   the specified predicate; otherwise, <b>False</b>.
    /// </returns>
    function Any(const predicate: Predicate<T>): Boolean; overload;

    /// <summary>
    ///   Concatenates two sequences.
    /// </summary>
    /// <param name="second">
    ///   The sequence to concatenate to the first sequence.
    /// </param>
    /// <returns>
    ///   An IEnumerable&lt;T&gt; that contains the concatenated elements of
    ///   the two input sequences.
    /// </returns>
    function Concat(const second: IEnumerable<T>): IEnumerable<T>;

    /// <summary>
    ///   Determines whether a sequence contains a specified element by using
    ///   the default equality comparer.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the sequence.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the source sequence contains an element that has the
    ///   specified value; otherwise, <b>False</b>.
    /// </returns>
    function Contains(const value: T): Boolean; overload;

    /// <summary>
    ///   Determines whether a sequence contains a specified element by using a
    ///   specified <see cref="IEqualityComparer&lt;T&gt;" />.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the sequence.
    /// </param>
    /// <param name="comparer">
    ///   An equality comparer to compare values.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the source sequence contains an element that has the
    ///   specified value; otherwise, <b>False</b>.
    /// </returns>
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;

    function Contains(const value: T; const comparer: TEqualityComparison<T>): Boolean; overload;

    /// <summary>
    ///   Returns the element at a specified index in a sequence.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to retrieve.
    /// </param>
    /// <returns>
    ///   The element at the specified position in the source sequence.
    /// </returns>
    function ElementAt(index: Integer): T;

    /// <summary>
    ///   Returns the element at a specified index in a sequence or a default
    ///   value if the index is out of range.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to retrieve.
    /// </param>
    /// <returns>
    ///   <b>Default(T)</b> if the index is outside the bounds of the
    ///   source sequence; otherwise, the element at the specified position in
    ///   the source sequence.
    /// </returns>
    function ElementAtOrDefault(index: Integer): T; overload;

    /// <summary>
    ///   Returns the element at a specified index in a sequence or the
    ///   specified default value if the index is out of range.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to retrieve.
    /// </param>
    /// <param name="defaultValue">
    ///   The value to return if the index is out of range.
    /// </param>
    /// <returns>
    ///   <i>DefaultValue</i> if the index is outside the bounds of the source
    ///   sequence; otherwise, the element at the specified position in the
    ///   source sequence.
    /// </returns>
    function ElementAtOrDefault(index: Integer; const defaultValue: T): T; overload;

    /// <summary>
    ///   Determines whether two sequences are equal by comparing the elements
    ///   by using the default equality comparer for their type.
    /// </summary>
    function EqualsTo(const values: array of T): Boolean; overload;

    /// <summary>
    ///   Determines whether two sequences are equal by comparing the elements
    ///   by using the default equality comparer for their type.
    /// </summary>
    function EqualsTo(const values: IEnumerable<T>): Boolean; overload;

    /// <summary>
    ///   Determines whether two sequences are equal by comparing their
    ///   elements by using a specified <c>IEqualityComparer&lt;T&gt;.</c>
    /// </summary>
    function EqualsTo(const values: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;

    /// <summary>
    ///   Returns the first element of a sequence.
    /// </summary>
    /// <returns>
    ///   The first element in the specified sequence.
    /// </returns>
    function First: T; overload;

    /// <summary>
    ///   Returns the first element in a sequence that satisfies a specified
    ///   condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   The first element in the sequence that passes the test in the
    ///   specified predicate function.
    /// </returns>
    function First(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the first element of a sequence, or a default value if the
    ///   sequence contains no elements.
    /// </summary>
    /// <returns>
    ///   <b>Default(T)</b> if source is empty; otherwise, the first
    ///   element in source.
    /// </returns>
    function FirstOrDefault: T; overload;

    /// <summary>
    ///   Returns the first element of a sequence, or the specified default
    ///   value if the sequence contains no elements.
    /// </summary>
    /// <param name="defaultValue">
    ///   The value to return if the sequence contains no elements.
    /// </param>
    /// <returns>
    ///   <i>DefaultValue</i> if source is empty; otherwise, the first element
    ///   in source.
    /// </returns>
    function FirstOrDefault(const defaultValue: T): T; overload;

    /// <summary>
    ///   Returns the first element of the sequence that satisfies a condition
    ///   or a default value if no such element is found.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <b>Default(T)</b> if source is empty or if no element passes
    ///   the test specified by predicate; otherwise, the first element in
    ///   source that passes the test specified by predicate.
    /// </returns>
    function FirstOrDefault(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the first element of the sequence that satisfies a condition
    ///   or the specified default value if no such element is found.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <param name="defaultValue">
    ///   The value to return if no element is found.
    /// </param>
    /// <returns>
    ///   <i>DefaultValue</i> if source is empty or if no element passes the
    ///   test specified by predicate; otherwise, the first element in source
    ///   that passes the test specified by predicate.
    /// </returns>
    function FirstOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    /// <summary>
    ///   Performs the specified action on each element of a sequence.
    /// </summary>
    procedure ForEach(const action: Action<T>);

    /// <summary>
    ///   Returns the last element of a sequence.
    /// </summary>
    /// <returns>
    ///   The value at the last position in the source sequence.
    /// </returns>
    function Last: T; overload;

    /// <summary>
    ///   Returns the last element of a sequence that satisfies a specified
    ///   condition.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   The last element in the sequence that passes the test in the
    ///   specified predicate function.
    /// </returns>
    function Last(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the last element of a sequence, or a default value if the
    ///   sequence contains no elements.
    /// </summary>
    /// <returns>
    ///   <b>Default(T)</b> if the source sequence is empty; otherwise,
    ///   the last element in the IEnumerable&lt;T&gt;.
    /// </returns>
    function LastOrDefault: T; overload;

    /// <summary>
    ///   Returns the last element of a sequence, or the specified default
    ///   value if the sequence contains no elements.
    /// </summary>
    /// <param name="defaultValue">
    ///   The value to return if the sequence contains no elements.
    /// </param>
    /// <returns>
    ///   <i>DefaultValue</i> if the source sequence is empty; otherwise, the
    ///   last element in the IEnumerable&lt;T&gt;.
    /// </returns>
    function LastOrDefault(const defaultValue: T): T; overload;

    /// <summary>
    ///   Returns the last element of a sequence that satisfies a condition or
    ///   a default value if no such element is found.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <returns>
    ///   <b>Default(T)</b> if the sequence is empty or if no elements
    ///   pass the test in the predicate function; otherwise, the last element
    ///   that passes the test in the predicate function.
    /// </returns>
    function LastOrDefault(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the last element of a sequence that satisfies a condition or
    ///   the specified default value if no such element is found.
    /// </summary>
    /// <param name="predicate">
    ///   A function to test each element for a condition.
    /// </param>
    /// <param name="defaultValue">
    ///   The value to return if no element is found.
    /// </param>
    /// <returns>
    ///   <i>DefaultValue</i> if the sequence is empty or if no elements pass
    ///   the test in the predicate function; otherwise, the last element that
    ///   passes the test in the predicate function.
    /// </returns>
    function LastOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    /// <summary>
    ///   Returns the maximum value in a sequence.
    /// </summary>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    function Max: T; overload;

    function Max(const selector: Func<T, Integer>): Integer; overload;

    /// <summary>
    ///   Returns the maximum value in a sequence by using the specified <see cref="IComparer&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="comparer">
    ///   An <see cref="IComparer&lt;T&gt;" /> to compare values.
    /// </param>
    /// <returns>
    ///   The maximum value in the sequence.
    /// </returns>
    function Max(const comparer: IComparer<T>): T; overload;
    function Max(const comparer: TComparison<T>): T; overload;

    /// <summary>
    ///   Returns the minimum value in a sequence.
    /// </summary>
    /// <returns>
    ///   The minimum value in the sequence.
    /// </returns>
    function Min: T; overload;

    function Min(const selector: Func<T, Integer>): Integer; overload;

    /// <summary>
    ///   Returns the minimum value in a sequence by using the specified <see cref="IComparer&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="comparer">
    ///   An <see cref="IComparer&lt;T&gt;" /> to compare values.
    /// </param>
    /// <returns>
    ///   The minimum value in the sequence.
    /// </returns>
    function Min(const comparer: IComparer<T>): T; overload;
    function Min(const comparer: TComparison<T>): T; overload;

    /// <summary>
    ///   Sorts the elements of a sequence in ascending order using the default
    ///   comparer for their type.
    /// </summary>
    function Ordered: IEnumerable<T>; overload;

    /// <summary>
    ///   Sorts the elements of a sequence in ascending order using the
    ///   specified <see cref="IComparer&lt;T&gt;" />.
    /// </summary>
    function Ordered(const comparer: IComparer<T>): IEnumerable<T>; overload;
    function Ordered(const comparer: TComparison<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Inverts the order of the elements in a sequence.
    /// </summary>
    function Reversed: IEnumerable<T>;

    /// <summary>
    ///   Returns the sequence in a shuffled order.
    /// </summary>
    function Shuffled: IEnumerable<T>;

    /// <summary>
    ///   Returns the only element of a sequence, and throws an exception if
    ///   there is not exactly one element in the sequence.
    /// </summary>
    function Single: T; overload;

    /// <summary>
    ///   Returns the only element of a sequence, and throws an exception if
    ///   there is not exactly one element in the sequence.
    /// </summary>
    function Single(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the only element of a sequence, or a default value if the
    ///   sequence is empty; this method throws an exception if there is more
    ///   than one element in the sequence.
    /// </summary>
    function SingleOrDefault: T; overload;

    /// <summary>
    ///   Returns the only element of a sequence, or the specified default
    ///   value if the sequence is empty; this method throws an exception if
    ///   there is more than one element in the sequence.
    /// </summary>
    function SingleOrDefault(const defaultValue: T): T; overload;

    /// <summary>
    ///   Returns the only element of a sequence that satisfies a specified
    ///   condition or a default value if no such element exists; this method
    ///   throws an exception if more than one element satisfies the condition.
    /// </summary>
    function SingleOrDefault(const predicate: Predicate<T>): T; overload;

    /// <summary>
    ///   Returns the only element of a sequence that satisfies a specified
    ///   condition or the specified default value if no such element exists;
    ///   this method throws an exception if more than one element satisfies
    ///   the condition.
    /// </summary>
    function SingleOrDefault(const predicate: Predicate<T>; const defaultValue: T): T; overload;

    /// <summary>
    ///   Bypasses a specified number of elements in a sequence and then
    ///   returns the remaining elements.
    /// </summary>
    function Skip(count: Integer): IEnumerable<T>;

    /// <summary>
    ///   Bypasses elements in a sequence as long as a specified condition is
    ///   true and then returns the remaining elements.
    /// </summary>
    function SkipWhile(const predicate: Predicate<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Bypasses elements in a sequence as long as a specified condition is
    ///   true and then returns the remaining elements. The element's index is
    ///   used in the logic of the predicate function.
    /// </summary>
    function SkipWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    /// <summary>
    ///   Computes the sum of the sequence.
    /// </summary>
    function Sum: T;
//    function Sum(const selector: Func<T, Integer>): Integer; overload;
//    function Sum(const selector: Func<T, Int64>): Int64; overload;
//    function Sum(const selector: Func<T, Double>): Double; overload;

    /// <summary>
    ///   Returns a specified number of contiguous elements from the start of a
    ///   sequence.
    /// </summary>
    function Take(count: Integer): IEnumerable<T>;

    /// <summary>
    ///   Returns elements from a sequence as long as a specified condition is
    ///   true.
    /// </summary>
    function TakeWhile(const predicate: Predicate<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Returns elements from a sequence as long as a specified condition is
    ///   true. The element's index is used in the logic of the predicate
    ///   function.
    /// </summary>
    function TakeWhile(const predicate: Func<T, Integer, Boolean>): IEnumerable<T>; overload;

    /// <summary>
    ///   Creates a new array which is filled with the elements in the
    ///   collection.
    /// </summary>
    function ToArray: TArray<T>;

    /// <summary>
    ///   Try getting the element at a specified index in a sequence.
    /// </summary>
    function TryGetElementAt(out value: T; index: Integer): Boolean;

    /// <summary>
    ///   Try getting the first element in a sequence.
    /// </summary>
    function TryGetFirst(out value: T): Boolean; overload;

    /// <summary>
    ///   Try getting the first element in a sequence that satisfies a
    ///   specified condition.
    /// </summary>
    function TryGetFirst(out value: T; const predicate: Predicate<T>): Boolean; overload;

    /// <summary>
    ///   Try getting the last element in a sequence.
    /// </summary>
    function TryGetLast(out value: T): Boolean; overload;

    /// <summary>
    ///   Try getting the last element in a sequence that satisfies a specified
    ///   condition.
    /// </summary>
    function TryGetLast(out value: T; const predicate: Predicate<T>): Boolean; overload;

    /// <summary>
    ///   Try getting the only element in a sequence.
    /// </summary>
    function TryGetSingle(out value: T): Boolean; overload;

    /// <summary>
    ///   Try getting the only element in a sequence that satisfies a specified
    ///   condition.
    /// </summary>
    function TryGetSingle(out value: T; const predicate: Predicate<T>): Boolean; overload;

    /// <summary>
    ///   Filters a sequence of values based on a predicate.
    /// </summary>
    function Where(const predicate: Predicate<T>): IEnumerable<T>; overload;

    /// <summary>
    ///   Gets the assigned comparer. If not comparer was assigned it returns
    ///   the default comparer.
    /// </summary>
    property Comparer: IComparer<T> read GetComparer;

    /// <summary>
    ///   Returns the number of elements in a sequence.
    /// </summary>
    /// <value>
    ///   The number of elements in the sequence.
    /// </value>
    property Count: Integer read GetCount;

    /// <summary>
    ///   Returns the type of the elements in the sequence.
    /// </summary>
    /// <value>
    ///   The type of the elements in the sequence.
    /// </value>
    property ElementType: PTypeInfo read GetElementType;

    /// <summary>
    ///   Determines whether the sequence contains no elements.
    /// </summary>
    /// <value>
    ///   <b>True</b> if the source sequence contains no elements; otherwise, <b>
    ///   False</b>.
    /// </value>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  /// <summary>
  ///   Represents a strongly-typed, read-only collection of elements.
  /// </summary>
  IReadOnlyCollection<T> = interface(IEnumerable<T>)
    ['{E1368FD5-02AE-4481-A9DC-96329DFF606C}']

    /// <summary>
    ///   Copies the elements of the collection to an array, starting at a
    ///   particular array index.
    /// </summary>
    /// <param name="values">
    ///   The one-dimensional array that is the destination of the elements
    ///   copied from the collection. The array must have zero-based indexing.
    /// </param>
    /// <param name="index">
    ///   The zero-based index in array at which copying begins.
    /// </param>
    procedure CopyTo(var values: TArray<T>; index: Integer);
  end;

  IReadOnlyList<T> = interface;

  /// <summary>
  ///   Defines methods to manipulate generic collections.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of the elements in the collection.
  /// </typeparam>
  ICollection<T> = interface(IEnumerable<T>)
    ['{9BFD9B06-45CD-4C80-B145-01B09D432CF0}']
  {$REGION 'Property Accessors'}
    function GetIsReadOnly: Boolean;
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    /// <summary>
    ///   Adds an item to the collection.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the collection
    /// </param>
    /// <returns>
    ///   True if the collection was modified, False otherwise.
    /// </returns>
    function Add(const item: T): Boolean;
    procedure AddRange(const values: array of T); overload;
    procedure AddRange(const values: IEnumerable<T>); overload;

    /// <summary>
    ///   Removes all items from the collection.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Copies the elements of the collection to an array, starting at a
    ///   particular array index.
    /// </summary>
    /// <param name="values">
    ///   The one-dimensional array that is the destination of the elements
    ///   copied from the collection. The array must have zero-based indexing.
    /// </param>
    /// <param name="index">
    ///   The zero-based index in array at which copying begins.
    /// </param>
    procedure CopyTo(var values: TArray<T>; index: Integer);

    /// <summary>
    ///   Moves the elements of the ICollection&lt;T&gt; to the specified
    ///   collection.
    /// </summary>
    /// <returns>
    ///   The number of elements that were moved.
    /// </returns>
    /// <remarks>
    ///   This internally uses Extract to make sure that objects from a list
    ///   with <b>OwnsObject</b> are not getting destroyed.
    /// </remarks>
    function MoveTo(const collection: ICollection<T>): Integer; overload;

    /// <summary>
    ///   Moves the elements of the ICollection&lt;T&gt; that are matching the
    ///   specified predicate to the specified collection.
    /// </summary>
    /// <returns>
    ///   The number of elements that were moved.
    /// </returns>
    /// <remarks>
    ///   This internally uses Extract to make sure that objects from a list
    ///   with <b>OwnsObject</b> are not getting destroyed.
    /// </remarks>
    function MoveTo(const collection: ICollection<T>;
      const predicate: Predicate<T>): Integer; overload;

    /// <summary>
    ///   Removes the first occurrence of a specific element from the
    ///   ICollection&lt;T&gt;.
    /// </summary>
    /// <param name="item">
    ///   The element to remove from the ICollection&lt;T&gt;.
    /// </param>
    /// <returns>
    ///   <b>True</b> if <i>item</i> was successfully removed from the
    ///   ICollection&lt;T&gt;; otherwise, <b>False</b>. This method also
    ///   returns <b>False</b> if <i>item</i> is not found in the original
    ///   ICollection&lt;T&gt;.
    /// </returns>
    function Remove(const item: T): Boolean;

    /// <summary>
    ///   Removes all the elements that match the conditions defined by the
    ///   specified predicate.
    /// </summary>
    /// <param name="predicate">
    ///   The predicate that defines the conditions of the elements to remove.
    /// </param>
    /// <returns>
    ///   The number of elements removed from the collection.
    /// </returns>
    function RemoveAll(const predicate: Predicate<T>): Integer;

    function RemoveRange(const values: array of T): Integer; overload;
    function RemoveRange(const values: IEnumerable<T>): Integer; overload;

    function Extract(const item: T): T;
    function ExtractAll(const predicate: Predicate<T>): TArray<T>;
    procedure ExtractRange(const values: array of T); overload;
    procedure ExtractRange(const values: IEnumerable<T>); overload;

    /// <summary>
    ///   Gets a value indicating whether the ICollection&lt;T&gt; is
    ///   read-only.
    /// </summary>
    /// <value>
    ///   <b>True</b> if the ICollection&lt;T&gt; is read-only; otherwise, <b>
    ///   False</b>.
    /// </value>
    /// <remarks>
    ///   A collection that is read-only does not allow the addition, removal,
    ///   or modification of elements after the collection is created.
    /// </remarks>
    property IsReadOnly: Boolean read GetIsReadOnly;
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  IList<T> = interface;

  /// <summary>
  ///   Represents a read-only collection of elements that can be accessed by
  ///   index.
  /// </summary>
  IReadOnlyList<T> = interface(IReadOnlyCollection<T>)
    ['{82A74ABB-509E-4AC0-9268-A993E7DC3AB3}']
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): T;
  {$ENDREGION}

    /// <summary>
    ///   Determines the index of a specific item in the
    ///   IReadOnlyList&lt;T&gt;.
    /// </summary>
    /// <param name="item">
    ///   The object to locate in the IReadOnlyList&lt;T&gt;.
    /// </param>
    /// <returns>
    ///   The index of <i>item</i> if found in the list; otherwise, -1.
    /// </returns>
    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    /// <summary>
    ///   Gets the element at the specified index in the read-only list.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the element to get.
    /// </param>
    /// <value>
    ///   The element at the specified index in the read-only list.
    /// </value>
    property Item[index: Integer]: T read GetItem; default;
  end;

  /// <summary>
  ///   Represents a collection of elements that can be individually accessed
  ///   by index.
  /// </summary>
  IList<T> = interface(ICollection<T>)
    ['{B6B4E1E1-0D29-40E1-854C-A93DEA8D1AA5}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(index: Integer): T;
    function GetOwnsObjects: Boolean;
    procedure SetCapacity(value: Integer);
    procedure SetCount(value: Integer);
    procedure SetItem(index: Integer; const item: T);
    procedure SetOwnsObjects(value: Boolean);
  {$ENDREGION}

    function Add(const item: T): Integer;

    /// <summary>
    ///   Inserts an item to the IList&lt;T&gt; at the specified index.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index at which item should be inserted.
    /// </param>
    /// <param name="item">
    ///   The element to insert into the IList&lt;T&gt;.
    /// </param>
    procedure Insert(index: Integer; const item: T);
    procedure InsertRange(index: Integer; const values: array of T); overload;
    procedure InsertRange(index: Integer; const values: IEnumerable<T>); overload;

    /// <summary>
    ///   Removes the item at the specified index.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the item to remove.
    /// </param>
    /// <exception cref="ArgumentOutOfRangeException">
    ///   <i>index</i> is not a valid index in the IList&lt;T&gt;.
    /// </exception>
    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    /// <summary>
    ///   Extracts the item at the specified index.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index of the item to extract.
    /// </param>
    /// <exception cref="ArgumentOutOfRangeException">
    ///   <i>index</i> is not a valid index in the IList&lt;T&gt;.
    /// </exception>
    function ExtractAt(index: Integer): T;
    function ExtractRange(index, count: Integer): TArray<T>; overload;

    /// <summary>
    ///   Creates a new list that contains a range of the elements in the
    ///   original list.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index at which the range starts.
    /// </param>
    /// <param name="count">
    ///   The number of elements in the range.
    /// </param>
    /// <remarks>
    ///   If the list contains reference types the elements in the returned
    ///   list point to the same instance as the elements in the original list.
    ///   Also if the original list is a <see cref="Spring.Collections.Lists|TObjectList&lt;T&gt;" />
    ///    it still owns the objects.
    /// </remarks>
    function GetRange(index, count: Integer): IList<T>;

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload;

    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(const comparer: TComparison<T>); overload;
    procedure Sort(const comparer: IComparer<T>; index, count: Integer); overload;
    procedure Sort(const comparer: TComparison<T>; index, count: Integer); overload;

    /// <summary>
    ///   Determines the index of a specific item in the IList&lt;T&gt;.
    /// </summary>
    /// <param name="item">
    ///   The element to locate in the IList&lt;T&gt;.
    /// </param>
    /// <returns>
    ///   The index of <i>item</i> if found in the list; otherwise, -1.
    /// </returns>
    /// <remarks>
    ///   If an element occurs multiple times in the list, the IndexOf method
    ///   always returns the first instance found.
    /// </remarks>
    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: T): Integer; overload;
    function LastIndexOf(const item: T; index: Integer): Integer; overload;
    function LastIndexOf(const item: T; index, count: Integer): Integer; overload;


    /// <summary>
    ///   Returns the list as read-only list.
    /// </summary>
    /// <remarks>
    ///   This method will not perform a copy but will return the same instance
    ///   as IReadOnlyList&lt;T&gt;.
    /// </remarks>
    function AsReadOnly: IReadOnlyList<T>;

    /// <summary>
    ///   Resize the internal storage so that it is the same size as the
    ///   collection.
    /// </summary>
    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[index: Integer]: T read GetItem write SetItem; default;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  IObjectList = interface(IList<TObject>)
    ['{78A32DC5-1A5B-4191-9CA5-006CD85CF1AA}']
    // DO NOT ADD ANY METHODS HERE!!!
  end;

  IInterfaceList = interface(IList<IInterface>)
    ['{B6BF9A6E-797C-4982-8D0D-B935E43D917E}']
    // DO NOT ADD ANY METHODS HERE!!!
  end;

  ILinkedList<T> = interface;

  /// <summary>
  ///   Represents a node in a <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
  ///    . This class cannot be inherited.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the element type of the linked list.
  /// </typeparam>
  TLinkedListNode<T> = class sealed
  protected
    fList: Pointer;
    {$IFDEF WEAKREF}
    // Indicates whether some list already incremented the refcount
    fOwned: Boolean;
    {$ENDIF}
    // Linked list keeps references locally
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fNext: TLinkedListNode<T>;
    {$IFDEF AUTOREFCOUNT}[Unsafe]{$ENDIF}
    fPrev: TLinkedListNode<T>;
    fItem: T;
    function GetList: ILinkedList<T>;
    function GetNext: TLinkedListNode<T>;
    function GetPrevious: TLinkedListNode<T>;
  public
    constructor Create(const value: T); overload;

    property List: ILinkedList<T> read GetList;

    /// <summary>
    ///   Gets the next node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <value>
    ///   A reference to the next node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    , or <b>nil</b> if the current node is the last element (Last) of
    ///   the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    property Next: TLinkedListNode<T> read GetNext;

    /// <summary>
    ///   Gets the previous node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <value>
    ///   A reference to the previous node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    , or null if the current node is the first element (First) of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </value>
    property Previous: TLinkedListNode<T> read GetPrevious;

    /// <summary>
    ///   Gets the value contained in the node.
    /// </summary>
    /// <value>
    ///   The value contained in the node.
    /// </value>
    property Value: T read fItem write fItem;
  end;

  /// <summary>
  ///   Represents a doubly linked list.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the element type of the linked list.
  /// </typeparam>
  ILinkedList<T> = interface(ICollection<T>)
    ['{73351AD9-15A5-4DA0-9BB7-D8FF66A3077E}']
  {$REGION 'Property Accessors'}
    function GetFirst: TLinkedListNode<T>;
    function GetLast: TLinkedListNode<T>;
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    /// <summary>
    ///   Adds the specified new node after the specified existing node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> after
    ///   which to insert <i>newNode</i>.
    /// </param>
    /// <param name="value">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <para>
    ///     <i>node</i> is <b>nil</b>.
    ///   </para>
    ///   <para>
    ///     -or-
    ///   </para>
    ///   <para>
    ///     <i>newNode</i> is <b>nil</b>.
    ///   </para>
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <para>
    ///     <i>node</i> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///      .
    ///   </para>
    ///   <para>
    ///     -or-
    ///   </para>
    ///   <para>
    ///     <i>newNode</i> belongs to another <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///      .
    ///   </para>
    /// </exception>
    procedure AddAfter(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Adds a new node containing the specified value after the specified
    ///   existing node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> after
    ///   which to insert a new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///    containing <i>value</i>.
    /// </param>
    /// <param name="value">
    ///   The value to add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <i>value</i>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    function AddAfter(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;

    /// <summary>
    ///   Adds the specified new node before the specified existing node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> before
    ///   which to insert <i>newNode</i>.
    /// </param>
    /// <param name="newNode">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <para>
    ///     <i>node</i> is <b>nil</b>.
    ///   </para>
    ///   <para>
    ///     -or-
    ///   </para>
    ///   <para>
    ///     <i>newNode</i> is <b>nil</b>.
    ///   </para>
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <para>
    ///     <i>node</i> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///      .
    ///   </para>
    ///   <para>
    ///     -or-
    ///   </para>
    ///   <para>
    ///     <i>newNode</i> belongs to another <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///      .
    ///   </para>
    /// </exception>
    procedure AddBefore(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Adds a new node containing the specified value before the specified
    ///   existing node in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> before
    ///   which to insert a new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///    containing <i>value</i>.
    /// </param>
    /// <param name="value">
    ///   The value to add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <i>value</i>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    function AddBefore(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;

    /// <summary>
    ///   Adds the specified new node at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> belongs to another <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    procedure AddFirst(const node: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Adds a new node containing the specified value at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="value">
    ///   The value to add at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <i>value</i>.
    /// </returns>
    function AddFirst(const value: T): TLinkedListNode<T>; overload;

    /// <summary>
    ///   Adds the specified new node at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> belongs to another <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    procedure AddLast(const node: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Adds a new node containing the specified value at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="value">
    ///   The value to add at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <i>value</i>.
    /// </returns>
    function AddLast(const value: T): TLinkedListNode<T>; overload;

    /// <summary>
    ///   Finds the first node that contains the specified value.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The first <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   that contains the specified value, if found; otherwise, <b>nil</b>.
    /// </returns>
    function Find(const value: T): TLinkedListNode<T>;

    /// <summary>
    ///   Finds the last node that contains the specified value.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <returns>
    ///   The last <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   that contains the specified value, if found; otherwise, <b>nil</b>.
    /// </returns>
    function FindLast(const value: T): TLinkedListNode<T>;

    /// <summary>
    ///   Removes the specified node from the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   remove from the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> is not in the current <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </exception>
    procedure Remove(const node: TLinkedListNode<T>); overload;

    /// <summary>
    ///   Removes the node at the start of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The <see cref="Spring.Collections|ILinkedList&lt;T&gt;" /> is empty.
    /// </exception>
    procedure RemoveFirst;

    /// <summary>
    ///   Removes the node at the end of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The <see cref="Spring.Collections|ILinkedList&lt;T&gt;" /> is empty.
    /// </exception>
    procedure RemoveLast;

    {$WARNINGS OFF}
    /// <summary>
    ///   Gets the first node of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <value>
    ///   The first <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    property First: TLinkedListNode<T> read GetFirst;

    /// <summary>
    ///   Gets the last node of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </summary>
    /// <value>
    ///   The last <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    property Last: TLinkedListNode<T> read GetLast;
    {$WARNINGS ON}

    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

//  TKeyValuePair<TKey, TValue> = record
//  private
//    fKey: TKey;
//    fValue: TValue;
//  public
//    constructor Create(const key: TKey; const value: TValue);
//
//    property Key: TKey read fKey;
//    property Value: TValue read fValue;
//  end;

  IReadOnlyMap<TKey, TValue> = interface(IReadOnlyCollection<TPair<TKey, TValue>>)
    ['{1FBECEB8-582E-4108-BB44-F21A06FE425B}']
  {$REGION 'Property Accessors'}
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetKeyType: PTypeInfo;
    function GetValues: IReadOnlyCollection<TValue>;
    function GetValueType: PTypeInfo;
  {$ENDREGION}

    /// <summary>
    ///   Determines whether the map contains the specified key/value pair.
    /// </summary>
    /// <param name="key">
    ///   The key of the pair to locate in the map.
    /// </param>
    /// <param name="value">
    ///   The value of the pair to locate in the map.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the map contains a pair with the specified key and
    ///   value; otherwise <b>False</b>.
    /// </returns>
    function Contains(const key: TKey; const value: TValue): Boolean;

    /// <summary>
    ///   Determines whether the map contains an element with the specified
    ///   key.
    /// </summary>
    /// <param name="key">
    ///   The key to locate in the map.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the map contains an element with the key; otherwise, <b>
    ///   False</b>.
    /// </returns>
    function ContainsKey(const key: TKey): Boolean;

    /// <summary>
    ///   Determines whether the map contains an element with the specified
    ///   value.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the map.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the map contains an element with the value; otherwise,
    ///   <b>False</b>.
    /// </returns>
    function ContainsValue(const value: TValue): Boolean;

    /// <summary>
    ///   Gets an <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the
    ///   keys of the map.
    /// </summary>
    /// <value>
    ///   An <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the keys of
    ///   the object that implements map.
    /// </value>
    property Keys: IReadOnlyCollection<TKey> read GetKeys;

    /// <summary>
    ///   Gets an <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the
    ///   values in the map.
    /// </summary>
    /// <value>
    ///   An <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the values
    ///   in the object that implements map.
    /// </value>
    property Values: IReadOnlyCollection<TValue> read GetValues;

    property KeyType: PTypeInfo read GetKeyType;
    property ValueType: PTypeInfo read GetValueType;
  end;

  /// <summary>
  ///   Represents a generic read-only collection of key/value pairs.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of keys in the read-only dictionary.
  /// </typeparam>
  /// <typeparam name="TValue">
  ///   The type of values in the read-only dictionary.
  /// </typeparam>
  IReadOnlyDictionary<TKey, TValue> = interface(IReadOnlyMap<TKey, TValue>)
    ['{39F7C68B-373E-4758-808C-705D3978E38F}']
  {$REGION 'Property Accessors'}
    function GetItem(const key: TKey): TValue;
  {$ENDREGION}

    /// <summary>
    ///   Gets the value for a given key if a matching key exists in the
    ///   dictionary; returns the default value otherwise.
    /// </summary>
    function GetValueOrDefault(const key: TKey): TValue; overload;

    /// <summary>
    ///   Gets the value for a given key if a matching key exists in the
    ///   dictionary; returns the given default value otherwise.
    /// </summary>
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;

    /// <summary>
    ///   Gets the value associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key whose value to get.
    /// </param>
    /// <param name="value">
    ///   When this method returns, the value associated with the specified
    ///   key, if the key is found; otherwise, the default value for the type
    ///   of the value parameter.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the object that implements IDictionary&lt;TKey,
    ///   TValue&gt; contains an element with the specified key; otherwise, <b>
    ///   False</b>.
    /// </returns>
    function TryGetValue(const key: TKey; out value: TValue): Boolean;

    /// <summary>
    ///   Gets the element that has the specified key in the read-only
    ///   dictionary.
    /// </summary>
    property Items[const key: TKey]: TValue read GetItem; default;
  end;

  /// <summary>
  ///   Represents a generic collection of key/value pairs.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of keys in the map.
  /// </typeparam>
  /// <typeparam name="TValue">
  ///   The type of values in the map.
  /// </typeparam>
  IMap<TKey, TValue> = interface(ICollection<TPair<TKey, TValue>>)
    ['{94262688-16E4-4092-926B-7B17FEF94A86}']
  {$REGION 'Property Accessors'}
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetKeyType: PTypeInfo;
    function GetOnKeyChanged: ICollectionChangedEvent<TKey>;
    function GetOnValueChanged: ICollectionChangedEvent<TValue>;
    function GetValues: IReadOnlyCollection<TValue>;
    function GetValueType: PTypeInfo;
  {$ENDREGION}

    /// <summary>
    ///   Adds an element with the provided key and value to the
    ///   IMap&lt;TKey,TValue&gt;.
    /// </summary>
    /// <param name="key">
    ///   The value to use as the key of the element to add.
    /// </param>
    /// <param name="value">
    ///   The value to use as the value of the element to add.
    /// </param>
    procedure Add(const key: TKey; const value: TValue);

    /// <summary>
    ///   Attempts to add the specified key and value to the map.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to add.
    /// </param>
    /// <param name="value">
    ///   The value of the element to add.
    /// </param>
    /// <returns>
    ///   True if the key/value pair was added to the map.
    /// </returns>
    function TryAdd(const key: TKey; const value: TValue): Boolean;

    /// <summary>
    ///   Removes the element with the specified key from the IMap&lt;TKey,
    ///   TValue&gt;.
    /// </summary>
    /// <param name="key">
    ///   The key of the element to remove.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the element is successfully removed; otherwise, <b>
    ///   False</b>. This method also returns <b>False</b> if <i>key</i> was
    ///   not found in the original IMap&lt;TKey, TValue&gt;.
    /// </returns>
    function Remove(const key: TKey): Boolean; overload;

    /// <summary>
    ///   Removes the specified key/value pair from the IMap&lt;TKey,
    ///   TValue&gt;.
    /// </summary>
    /// <param name="key">
    ///   The key of the pair to remove.
    /// </param>
    /// <param name="value">
    ///   The value of the pair to remove,
    /// </param>
    /// <returns>
    ///   <b>True</b> if the pair was successfully removed; otherwise, <b>False</b>
    ///    .
    /// </returns>
    function Remove(const key: TKey; const value: TValue): Boolean; overload;

    function Extract(const key: TKey; const value: TValue): TPair<TKey, TValue>;

    /// <summary>
    ///   Determines whether the IMap&lt;TKey,TValue&gt; contains the specified
    ///   key/value pair.
    /// </summary>
    /// <param name="key">
    ///   The key of the pair to locate in the IMap&lt;TKey, TValue&gt;.
    /// </param>
    /// <param name="value">
    ///   The value of the pair to locate in the IMap&lt;TKey, TValue&gt;.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the IMap&lt;TKey, TValue&gt; contains a pair with the
    ///   specified key and value; otherwise <b>False</b>.
    /// </returns>
    function Contains(const key: TKey; const value: TValue): Boolean; overload;

    /// <summary>
    ///   Determines whether the IMap&lt;TKey, TValue&gt; contains an element
    ///   with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key to locate in the IMap&lt;TKey, TValue&gt;.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the IMap&lt;TKey, TValue&gt; contains an element with
    ///   the key; otherwise, <b>False</b>.
    /// </returns>
    function ContainsKey(const key: TKey): Boolean;

    /// <summary>
    ///   Determines whether the IMap&lt;TKey, TValue&gt; contains an element
    ///   with the specified value.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the IMap&lt;TKey, TValue&gt;.
    /// </param>
    function ContainsValue(const value: TValue): Boolean;

    /// <summary>
    ///   Gets an <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the
    ///   keys of the IMap&lt;TKey, TValue&gt;.
    /// </summary>
    /// <value>
    ///   An <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the keys of
    ///   the object that implements IDictionary&lt;TKey, TValue&gt;.
    /// </value>
    property Keys: IReadOnlyCollection<TKey> read GetKeys;

    /// <summary>
    ///   Gets an <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the
    ///   values in the IMap&lt;TKey, TValue&gt;.
    /// </summary>
    /// <value>
    ///   An <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the values
    ///   in the object that implements IMap&lt;TKey, TValue&gt;.
    /// </value>
    property Values: IReadOnlyCollection<TValue> read GetValues;

    property OnKeyChanged: ICollectionChangedEvent<TKey> read GetOnKeyChanged;
    property OnValueChanged: ICollectionChangedEvent<TValue> read GetOnValueChanged;
    property KeyType: PTypeInfo read GetKeyType;
    property ValueType: PTypeInfo read GetValueType;
  end;

  /// <summary>
  ///   Represents a generic collection of key/value pairs.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of keys in the dictionary.
  /// </typeparam>
  /// <typeparam name="TValue">
  ///   The type of values in the dictionary.
  /// </typeparam>
  IDictionary<TKey, TValue> = interface(IMap<TKey, TValue>)
    ['{7F0D544F-6A59-4FA0-9C96-DB09029CC835}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetItem(const key: TKey): TValue;
    procedure SetCapacity(value: Integer);
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}

    procedure AddOrSetValue(const key: TKey; const value: TValue); deprecated 'Use dict[key] := value instead';

    /// <summary>
    ///   Removes the value for a specified key without triggering lifetime
    ///   management for objects.
    /// </summary>
    /// <param name="key">
    ///   The key whose value to remove.
    /// </param>
    /// <returns>
    ///   The removed value for the specified key if it existed; <b>default</b>
    ///   otherwise.
    /// </returns>
    function Extract(const key: TKey): TValue; overload;

    /// <summary>
    ///   Gets the value for a given key if a matching key exists in the
    ///   dictionary; returns the default value otherwise.
    /// </summary>
    function GetValueOrDefault(const key: TKey): TValue; overload;

    /// <summary>
    ///   Gets the value for a given key if a matching key exists in the
    ///   dictionary; returns the given default value otherwise.
    /// </summary>
    function GetValueOrDefault(const key: TKey; const defaultValue: TValue): TValue; overload;

    /// <summary>
    ///   Attempts to get and remove the value associated with the specified
    ///   key, without triggering lifetime management for objects.
    /// </summary>
    /// <param name="key">
    ///   The key whose value to get.
    /// </param>
    /// <param name="value">
    ///   The value associated with the specified key, if the key is found;
    ///   otherwise, <b>Default(TValue)</b>.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the key is found; <b>False</b> otherwise.
    /// </returns>
    function TryExtract(const key: TKey; out value: TValue): Boolean;

    /// <summary>
    ///   Gets the value associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key whose value to get.
    /// </param>
    /// <param name="value">
    ///   The value associated with the specified key, if the key is found;
    ///   otherwise, <b>Default(TValue)</b>.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the key is found; <b>False</b> otherwise.
    /// </returns>
    function TryGetValue(const key: TKey; out value: TValue): Boolean;

    /// <summary>
    ///   Updates the value associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key whose value to update.
    /// </param>
    /// <param name="newValue">
    ///   The new value to be associated with the specified key, if the key is found.
    /// </param>
    /// <param name="oldValue">
    ///   The original value associated with the specified key, if the key is found;
    ///   otherwise, <b>Default(TValue)</b>.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the key is found and the value updated; <b>False</b> otherwise.
    /// </returns>
    function TryUpdateValue(const key: TKey; const newValue: TValue; out value: TValue): Boolean;

    /// <summary>
    ///   Returns the dictionary as read-only dictionary.
    /// </summary>
    /// <remarks>
    ///   This method will not perform a copy but will return the same instance
    ///   as IReadOnlyDictionary&lt;TKey, TValue&gt;.
    /// </remarks>
    function AsReadOnly: IReadOnlyDictionary<TKey, TValue>;

    /// <summary>
    ///   Resize the internal storage so that it is the same size as the
    ///   collection.
    /// </summary>
    procedure TrimExcess;

    /// <summary>
    ///   Gets or sets the size of the internal storage.
    /// </summary>
    property Capacity: Integer read GetCapacity write SetCapacity;

    /// <summary>
    ///   Gets or sets the value associated with the specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the value to get or set.
    /// </param>
    /// <value>
    ///   The value associated with the specified key. If the specified key is
    ///   not found, a get operation throws an EKeyNotFoundException, and a set
    ///   operation creates a new element with the specified key.
    /// </value>
    /// <exception cref="EKeyNotFoundException">
    ///   The property is retrieved and <i>key</i> does not exist in the
    ///   collection.
    /// </exception>
    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  end;

  IBidiDictionary<TKey, TValue> = interface(IDictionary<TKey, TValue>)
    ['{DA8F1C48-B4F4-4487-ADAD-AF15596DD53C}']
  {$REGION 'Property Accessors'}
    function GetInverse: IBidiDictionary<TValue, TKey>;
  {$ENDREGION}

    /// <summary>
    ///   Returns the inverse view of this bidirectional dictionary which maps
    ///   each of its values to its associated key.
    /// </summary>
    property Inverse: IBidiDictionary<TValue, TKey> read GetInverse;
  end;

  IReadOnlyMultiMap<TKey, TValue> = interface(IReadOnlyMap<TKey, TValue>)
    ['{5411F9EC-5A56-4F40-890A-089A08AE795F}']
  {$REGION 'Property Accessors'}
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
  {$ENDREGION}

    function TryGetValues(const key: TKey; out values: IReadOnlyCollection<TValue>): Boolean;
    property Items[const key: TKey]: IReadOnlyCollection<TValue> read GetItems; default;
  end;

  IMultiMap<TKey, TValue> = interface(IMap<TKey, TValue>)
    ['{8598095E-92A7-4FCC-9F78-8EE7653B8B49}']
  {$REGION 'Property Accessors'}
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
  {$ENDREGION}

    /// <summary>
    ///   Adds an element with the provided key and value to the map and
    ///   returns if the underlying map was modified.
    /// </summary>
    /// <param name="key">
    ///   The value to use as the key of the element to add.
    /// </param>
    /// <param name="value">
    ///   The value to use as the value of the element to add.
    /// </param>
    /// <returns>
    ///   True if the map was modified, False otherwise.
    /// </returns>
    function Add(const key: TKey; const value: TValue): Boolean;

    procedure AddRange(const key: TKey; const values: array of TValue); overload;
    procedure AddRange(const key: TKey; const values: IEnumerable<TValue>); overload;

    /// <summary>
    ///   Extracts all values for the given key from the multimap.
    /// </summary>
    /// <remarks>
    ///   If the multimap has doOwnsValues set the items in the returned list
    ///   are not being owned by the list but have to be freed manually or
    ///   being passed to a collection that takes ownership.
    /// </remarks>
    function Extract(const key: TKey): ICollection<TValue>; overload;

    function TryGetValues(const key: TKey; out values: IReadOnlyCollection<TValue>): Boolean;

    /// <summary>
    ///   Returns the multimap as read-only multimap.
    /// </summary>
    /// <remarks>
    ///   This method will not perform a copy but will return the same instance
    ///   as IReadOnlyMultiMap&lt;TKey, TValue&gt;.
    /// </remarks>
    function AsReadOnly: IReadOnlyMultiMap<TKey, TValue>;

    property Items[const key: TKey]: IReadOnlyCollection<TValue> read GetItems; default;
  end;

  /// <summary>
  ///   Represents a variable size last-in-first-out (LIFO) collection of
  ///   instances of the same arbitrary type.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the stack.
  /// </typeparam>
  IStack<T> = interface(IEnumerable<T>)
    ['{5BD7BDD3-0198-4727-B97C-658BF194FF63}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetOnChanged: ICollectionChangedEvent<T>;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Removes all elements from the stack.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Adds an element at the top of the stack.
    /// </summary>
    /// <param name="item">
    ///   The element to push onto the stack. The value can be <b>nil</b> for
    ///   reference types.
    /// </param>
    /// <returns>
    ///   True if the element was added, False otherwise.
    /// </returns>
    function Push(const item: T): Boolean;

    /// <summary>
    ///   Removes the element at the top of the stack.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    /// <remarks>
    ///   This will return nil to prevent a dangling reference if the stack has
    ///   ownership over the instances.
    /// </remarks>
    function Pop: T;

    /// <summary>
    ///   Removes and returns the element at the top of the stack. If the
    ///   stack has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    function Extract: T;

    /// <summary>
    ///   Returns the element at the top of the stack without removing it.
    /// </summary>
    function Peek: T;

    /// <summary>
    ///   Returns the element at the top of the stack without removing it.
    ///   Returns <b>Default(T)</b> if the stack is empty.
    /// </summary>
    function PeekOrDefault: T;

    /// <summary>
    ///   Attempts to remove and return the element at the top of the stack.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful and
    ///   the stack does not have ownership of the instances; <b>Default(T)</b>
    ///   otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was removed; otherwise, <b>False</b>.
    /// </returns>
    function TryPop(out item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the top of the stack.
    ///   If the stack has ownership over the instances, then ownership of
    ///   the returned element is transferred to the caller.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful; <b>
    ///   Default(T)</b> otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was removed; otherwise, <b>False</b>.
    /// </returns>
    function TryExtract(out item: T): Boolean;

    /// <summary>
    ///   Attempts to return an element from the top of the stack without
    ///   removing it.
    /// </summary>
    /// <param name="item">
    ///   The element at the top of the stack if the operation was
    ///   successful; <b>Default(T)</b> otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was returned; otherwise, <b>False</b>.
    /// </returns>
    function TryPeek(out item: T): Boolean;

    /// <summary>
    ///   Resize the internal storage so that it is the same size as the
    ///   collection.
    /// </summary>
    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  /// <summary>
  ///   Represents a first-in, first-out collection of elements.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the queue.
  /// </typeparam>
  IQueue<T> = interface(IEnumerable<T>)
    ['{D305A076-3F19-497C-94E3-6BD1C7A30F3F}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetOnChanged: ICollectionChangedEvent<T>;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Removes all elements from the queue.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Adds an element to the end of the queue.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the queue. The value can be <b>nil</b> for
    ///   reference types.
    /// </param>
    /// <returns>
    ///   True if the element was added, False otherwise.
    /// </returns>
    function Enqueue(const item: T): Boolean;

    /// <summary>
    ///   Removes the element at the beginning of the queue.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    /// <remarks>
    ///   This will return nil to prevent a dangling reference if the queue has
    ///   ownership over the instances.
    /// </remarks>
    function Dequeue: T;

    /// <summary>
    ///   Removes and returns the element at the beginning of the queue. If the
    ///   queue has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    function Extract: T;

    /// <summary>
    ///   Returns the element at the beginning of the queue without removing
    ///   it.
    /// </summary>
    function Peek: T;

    /// <summary>
    ///   Returns the element at the beginning of the queue without removing
    ///   it. Returns <b>Default(T)</b> if the queue is empty.
    /// </summary>
    function PeekOrDefault: T;

    /// <summary>
    ///   Attempts to remove and return the element at the beginning of the
    ///   queue.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful and
    ///   the queue does not have ownership of the instances; <b>Default(T)</b>
    ///   otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was removed; otherwise, <b>False</b>.
    /// </returns>
    function TryDequeue(out item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the beginning of the
    ///   queue. If the queue has ownership over the instances, then ownership
    ///   of the returned element is transferred to the caller.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful; <b>
    ///   Default(T)</b> otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was removed; otherwise, <b>False</b>.
    /// </returns>
    function TryExtract(out item: T): Boolean;

    /// <summary>
    ///   Attempts to return an element from the beginning of the queue without
    ///   removing it.
    /// </summary>
    /// <param name="item">
    ///   The element at the beginning of the queue if the operation was
    ///   successful; <b>Default(T)</b> otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was returned; otherwise, <b>False</b>.
    /// </returns>
    function TryPeek(out item: T): Boolean;

    /// <summary>
    ///   Resize the internal storage so that it is the same size as the
    ///   collection.
    /// </summary>
    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  /// <summary>
  ///   Represents a double-ended queue.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the type of elements in the deque.
  /// </typeparam>
  IDeque<T> = interface(IEnumerable<T>)
    ['{852D8B1A-8587-4C7E-85DA-41F255887153}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetOnChanged: ICollectionChangedEvent<T>;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Removes all elements from the deque.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Adds an element to the front of the deque.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the deque. The value can be <b>nil</b> for
    ///   reference types.
    /// </param>
    /// <returns>
    ///   True if the element was added, False otherwise.
    /// </returns>
    function AddFirst(const item: T): Boolean;

    /// <summary>
    ///   Adds an element to the back of the deque.
    /// </summary>
    /// <param name="item">
    ///   The element to add to the deque. The value can be <b>nil</b> for
    ///   reference types.
    /// </param>
    /// <returns>
    ///   True if the element was added, False otherwise.
    /// </returns>
    function AddLast(const item: T): Boolean;

    /// <summary>
    ///   Removes the element at the front of the deque.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    /// <remarks>
    ///   This will return nil to prevent a dangling reference if the deque has
    ///   ownership over the instances.
    /// </remarks>
    function RemoveFirst: T;

    /// <summary>
    ///   Removes the element at the back of the deque.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    /// <remarks>
    ///   This will return nil to prevent a dangling reference if the deque has
    ///   ownership over the instances.
    /// </remarks>
    function RemoveLast: T;

    /// <summary>
    ///   Removes and returns the element at the front of the deque. If the
    ///   deque has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    function ExtractFirst: T;

    /// <summary>
    ///   Removes and returns the element at the back of the deque. If the
    ///   deque has ownership over the instances, then ownership of the
    ///   returned element is transferred to the caller.
    /// </summary>
    /// <returns>
    ///   The element that was removed.
    /// </returns>
    function ExtractLast: T;

    /// <summary>
    ///   Attempts to remove and return the element at the front of the
    ///   deque.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful and
    ///   the deque does not have ownership of the instances; <b>Default(T)</b>
    ///   otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was removed; otherwise, <b>False</b>.
    /// </returns>
    function TryRemoveFirst(out item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the back of the
    ///   deque.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful and
    ///   the deque does not have ownership of the instances; <b>Default(T)</b>
    ///   otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was removed; otherwise, <b>False</b>.
    /// </returns>
    function TryRemoveLast(out item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the front of the
    ///   deque. If the deque has ownership over the instances, then ownership
    ///   of the returned element is transferred to the caller.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful; <b>
    ///   Default(T)</b> otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was removed; otherwise, <b>False</b>.
    /// </returns>
    function TryExtractFirst(out item: T): Boolean;

    /// <summary>
    ///   Attempts to remove and return the element at the back of the
    ///   deque. If the deque has ownership over the instances, then ownership
    ///   of the returned element is transferred to the caller.
    /// </summary>
    /// <param name="item">
    ///   The element that was removed if the operation was successful; <b>
    ///   Default(T)</b> otherwise.
    /// </param>
    /// <returns>
    ///   <b>True</b> if an element was removed; otherwise, <b>False</b>.
    /// </returns>
    function TryExtractLast(out item: T): Boolean;

    /// <summary>
    ///   Resize the internal storage so that it is the same size as the
    ///   collection.
    /// </summary>
    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  /// <summary>
  ///   Provides the base interface for the abstraction of sets.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements in the set.
  /// </typeparam>
  ISet<T> = interface(ICollection<T>)
    ['{DC0B211F-E9FD-41D6-BEE0-FCB9F79327AB}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    procedure SetCapacity(value: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Removes all elements in the specified collection from the current
    ///   set.
    /// </summary>
    /// <param name="other">
    ///   The collection of items to remove from the set.
    /// </param>
    /// <exception cref="EArgumentNullException">
    ///   <i>other</i> is <b>nil</b>.
    /// </exception>
    procedure ExceptWith(const other: IEnumerable<T>);

    /// <summary>
    ///   Modifies the current set so that it contains only elements that are
    ///   also in a specified collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <exception cref="EArgumentNullException">
    ///   <i>other</i> is <b>nil</b>.
    /// </exception>
    procedure IntersectWith(const other: IEnumerable<T>);

    /// <summary>
    ///   Modifies the current set so that it contains all elements that are
    ///   present in either the current set or the specified collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <exception cref="EArgumentNullException">
    ///   <i>other</i> is <b>nil</b>.
    /// </exception>
    procedure UnionWith(const other: IEnumerable<T>);

    /// <summary>
    ///   Determines whether a set is a subset of a specified collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the current set is a subset of <i>other</i>;
    ///   otherwise, <b>False</b>.
    /// </returns>
    /// <exception cref="EArgumentNullException">
    ///   <i>other</i> is <b>nil</b>.
    /// </exception>
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Determines whether the current set is a superset of a specified
    ///   collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the current set is a superset of <i>other</i>;
    ///   otherwise, <b>False</b>.
    /// </returns>
    /// <exception cref="EArgumentNullException">
    ///   <i>other</i> is <b>nil</b>.
    /// </exception>
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Determines whether the current set and the specified collection
    ///   contain the same elements.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the current set is equal to <i>other</i>; otherwise, <b>
    ///   False</b>.
    /// </returns>
    /// <exception cref="EArgumentNullException">
    ///   <i>other</i> is <b>nil</b>.
    /// </exception>
    function SetEquals(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Determines whether the current set overlaps with the specified
    ///   collection.
    /// </summary>
    /// <param name="other">
    ///   The collection to compare to the current set.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the current set and <i>other</i> share at least one
    ///   common element; otherwise, <b>False</b>.
    /// </returns>
    /// <exception cref="EArgumentNullException">
    ///   <i>other</i> is <b>nil</b>.
    /// </exception>
    function Overlaps(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Resize the internal storage so that it is the same size as the
    ///   collection.
    /// </summary>
    procedure TrimExcess;

    /// <summary>
    ///   Gets or sets the size of the internal storage.
    /// </summary>
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  TMultiSetEntry<T> = record
    Item: T;
    Count: Integer;
  end;

  IReadOnlyMultiSet<T> = interface(IReadOnlyCollection<T>)
    ['{7ECC0F3E-B73C-4821-82ED-FD84E0F81856}']
  {$REGION 'Property Accessors'}
    function GetEntries: IReadOnlyCollection<TMultiSetEntry<T>>;
    function GetItems: IReadOnlyCollection<T>;
    function GetItemCount(const item: T): Integer;
  {$ENDREGION}

    /// <summary>
    ///   Returns a view of the contents of this multiset, each providing an
    ///   element of the multiset and the count of that element. This
    ///   collection contains exactly one entry for each distinct element in
    ///   the multiset (thus it always has the same size as Elements).
    /// </summary>
    property Entries: IReadOnlyCollection<TMultiSetEntry<T>> read GetEntries;

    /// <summary>
    ///   Returns the collection of distinct elements contained in this
    ///   multiset.
    /// </summary>
    property Items: IReadOnlyCollection<T> read GetItems;

    /// <summary>
    ///   Returns the number of occurrences of an element in this multiset (the
    ///   count of the element).
    /// </summary>
    property ItemCount[const item: T]: Integer read GetItemCount; default;
  end;

  IMultiSet<T> = interface(ICollection<T>)
    ['{CC7C2115-EED6-4FDE-9AE6-44C253514B2F}']
  {$REGION 'Property Accessors'}
    function GetEntries: IReadOnlyCollection<TMultiSetEntry<T>>;
    function GetItems: IReadOnlyCollection<T>;
    function GetItemCount(const item: T): Integer;
    procedure SetItemCount(const item: T; count: Integer);
  {$ENDREGION}

    /// <summary>
    ///   Adds a number of occurrences of an element to the multiset.
    /// </summary>
    /// <param name="item">
    ///   The element to add occurrences of
    /// </param>
    /// <param name="count">
    ///   The number of occurrences of the element to add
    /// </param>
    /// <returns>
    ///   The count of the element before the operation, zero if the element
    ///   was not in the multiset.
    /// </returns>
    /// <exception cref="EInvalidArgumentException">
    ///   If count is negative
    /// </exception>
    function Add(const item: T; count: Integer): Integer; overload;

    /// <summary>
    ///   Removes a number of occurrences of an element from the multiset. If
    ///   the multiset contains fewer than this number of occurrences to begin
    ///   with, all occurrences will be removed.
    /// </summary>
    /// <param name="item">
    ///   The element to remove occurrences of
    /// </param>
    /// <param name="count">
    ///   The count of occurrences of the element to remove
    /// </param>
    /// <returns>
    ///   The count of the element before the operation, zero if the element
    ///   was not in the multiset.
    /// </returns>
    /// <exception cref="EInvalidArgumentException">
    ///   If count is negative
    /// </exception>
    function Remove(const item: T; count: Integer): Integer; overload;

    /// <summary>
    ///   Determines whether the current multiset and the specified collection
    ///   contain the same number of elements.
    /// </summary>
    function SetEquals(const other: IEnumerable<T>): Boolean;

    /// <summary>
    ///   Returns a read-only view on the multiset that is ordered by the
    ///   occurences of its items starting with the highest occurence.
    /// </summary>
    function OrderedByCount: IReadOnlyMultiSet<T>;

    /// <summary>
    ///   Returns a view of the contents of this multiset, each providing an
    ///   element of the multiset and the count of that element. This
    ///   collection contains exactly one entry for each distinct element in
    ///   the multiset (thus it always has the same size as Elements).
    /// </summary>
    property Entries: IReadOnlyCollection<TMultiSetEntry<T>> read GetEntries;

    /// <summary>
    ///   Returns the collection of distinct elements contained in this
    ///   multiset.
    /// </summary>
    property Items: IReadOnlyCollection<T> read GetItems;

    /// <summary>
    ///   Returns or sets the number of occurrences of an element in this
    ///   multiset (the count of the element).
    /// </summary>
    property ItemCount[const item: T]: Integer read GetItemCount write SetItemCount; default;
  end;

  /// <summary>
  ///   Represents a collection of elements that have a common key.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of the key of the IGrouping&lt;TKey, TElement&gt;.
  /// </typeparam>
  /// <typeparam name="TElement">
  ///   The type of the values in the IGrouping&lt;TKey, TElement&gt;.
  /// </typeparam>
  IGrouping<TKey, TElement> = interface(IEnumerable<TElement>)
    ['{CFC3071C-663A-400A-B21B-1F5E28BA4892}']
  {$REGION 'Property Accessors'}
    function GetKey: TKey;
  {$ENDREGION}

    /// <summary>
    ///   Gets the key of the IGrouping&lt;TKey, TElement&gt;.
    /// </summary>
    /// <value>
    ///   The key of the IGrouping&lt;TKey, TElement&gt;.
    /// </value>
    property Key: TKey read GetKey;
  end;

  /// <summary>
  ///   Defines an indexer, size property, and Boolean search method for data
  ///   structures that map keys to <see cref="IEnumerable&lt;T&gt;" />
  ///   sequences of values.
  /// </summary>
  /// <typeparam name="TKey">
  ///   The type of the keys in the ILookup&lt;TKey, TElement&gt;.
  /// </typeparam>
  /// <typeparam name="TElement">
  ///   The type of the elements in the <see cref="IEnumerable&lt;T&gt;" />
  ///   sequences that make up the values in the ILookup&lt;TKey, TElement&gt;.
  /// </typeparam>
  ILookup<TKey, TElement> = interface(IEnumerable<IGrouping<TKey, TElement>>)
    ['{B2380533-F2B1-465B-84B2-97FA79A6EE09}']
  {$REGION 'Property Accessors'}
    function GetItem(const key: TKey): IEnumerable<TElement>;
  {$ENDREGION}

    /// <summary>
    ///   Determines whether a specified key exists in the ILookup&lt;TKey,
    ///   TElement&gt;.
    /// </summary>
    /// <param name="key">
    ///   The key to search for in the ILookup&lt;TKey, TElement&gt;.
    /// </param>
    /// <returns>
    ///   <b>True</b> if <i>key</i> is in the ILookup&lt;TKey, TElement&gt;;
    ///   otherwise, <b>False</b>.
    /// </returns>
    function Contains(const key: TKey): Boolean;

    /// <summary>
    ///   Gets the <see cref="IEnumerable&lt;T&gt;" /> sequence of values
    ///   indexed by a specified key.
    /// </summary>
    /// <param name="key">
    ///   The key of the desired sequence of values.
    /// </param>
    /// <value>
    ///   The <see cref="IEnumerable&lt;T&gt;" /> sequence of values indexed by
    ///   the specified key.
    /// </value>
    property Item[const key: TKey]: IEnumerable<TElement> read GetItem; default;
  end;

  /// <summary>
  ///   Provides direct access to an array that is used for internal storage.
  /// </summary>
  IArrayAccess<T> = interface(ICountable)
    ['{0C6C22BE-DBFD-4EBE-9E32-6E4BBA8AC382}']
  {$REGION 'Property Accessors'}
     function GetItems: TArray<T>;
  {$ENDREGION}

    property Items: TArray<T> read GetItems;
  end;

  /// <summary>
  ///   Provides static methods to create an instance of various interfaced
  ///   generic collections such as <see cref="IList&lt;T&gt;" /> or <see cref="IDictionary&lt;TKey, TValue&gt;" />
  ///    .
  /// </summary>
  TCollections = class
  strict protected
    const FoldedTypeKinds = [tkInteger, tkChar, tkEnumeration, tkWChar, tkInt64, tkUString, {$IFNDEF AUTOREFCOUNT}tkClass, {$ENDIF}tkClassRef, tkPointer, tkProcedure, tkInterface];

    class procedure CreateDictionary_UInt8_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt8_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt8_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt8_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt8_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt8_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_UInt16_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt16_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt16_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt16_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt16_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt16_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_UInt32_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt32_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt32_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt32_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt32_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt32_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_UInt64_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt64_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt64_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt64_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt64_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_UInt64_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_Interface_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_Interface_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateDictionary_String_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateDictionary_String_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_UInt8_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt8_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt8_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt8_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt8_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt8_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_UInt16_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt16_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt16_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt16_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt16_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt16_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_UInt32_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt32_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt32_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt32_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt32_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt32_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_UInt64_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt64_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt64_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt64_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt64_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_UInt64_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_Interface_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_Interface_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateBidiDictionary_String_UInt8(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_UInt16(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_UInt32(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_UInt64(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_Interface(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateBidiDictionary_String_String(capacity: Integer;
      keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_UInt8_UInt8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt8_UInt16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt8_UInt32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt8_UInt64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt8_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt8_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_UInt16_UInt8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt16_UInt16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt16_UInt32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt16_UInt64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt16_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt16_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_UInt32_UInt8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt32_UInt16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt32_UInt32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt32_UInt64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt32_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt32_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_UInt64_UInt8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt64_UInt16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt64_UInt32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt64_UInt64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt64_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_UInt64_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_Interface_UInt8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_UInt16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_UInt32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_UInt64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_Interface_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateListMultiMap_String_UInt8(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_UInt16(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_UInt32(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_UInt64(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_Interface(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;
    class procedure CreateListMultiMap_String_String(
      keyComparer: Pointer; ownerships: TDictionaryOwnerships;
      var result; keyType, valueType, elementType: PTypeInfo); static;

    class procedure CreateList_UInt8(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_UInt16(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_UInt32(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_UInt64(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_Method(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateList_Interface(var result; elementType: Pointer); overload; static;
    class procedure CreateList_Interface(comparer: Pointer; var result; elementType: Pointer); overload; static;
    class procedure CreateList_Object(var result; elementType: Pointer); overload; static;
    class procedure CreateList_Object(ownsObjects: Boolean; var result; elementType: Pointer); overload; static;
    class procedure CreateList_Object(comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer); overload; static;
    class procedure CreateList_String(comparer: Pointer; var result; elementType: Pointer); static;

    class procedure CreateSortedList_UInt8(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_UInt16(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_UInt32(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_UInt64(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_Interface(comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateSortedList_Object(comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer); static;
    class procedure CreateSortedList_String(comparer: Pointer; var result; elementType: Pointer); static;

    class procedure CreateHashSet_UInt8(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_UInt16(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_UInt32(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_UInt64(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_Object(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateHashSet_String(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;

    class procedure CreateQueue_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateQueue_Object(capacity: Integer; comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer); static;

    class procedure CreateStack_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer); static;
    class procedure CreateStack_Object(capacity: Integer; comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer); static;

    class procedure CreateObservableList_Interface(var result; elementType: Pointer); static;
    class procedure CreateObservableList_Object(ownsObjects: Boolean; var result; elementType: Pointer); static;
  public
    class function CreateList<T>: IList<T>; overload; static;
    class function CreateList<T>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateList<T>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateList<T>(const values: array of T): IList<T>; overload; static;
    class function CreateList<T>(const values: IEnumerable<T>): IList<T>; overload; static;
    class function CreateList<T: class>(ownsObjects: Boolean): IList<T>; overload; static; deprecated 'Use CreateObjectList instead';
    class function CreateList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean): IList<T>; overload; static; deprecated 'Use CreateObjectList instead';
    class function CreateList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean): IList<T>; overload; static; deprecated 'Use CreateObjectList instead';

    class function CreateObjectList(elementType: PTypeInfo; ownsObjects: Boolean): IObjectList; overload; static;
    class function CreateObjectList<T: class>: IList<T>; overload; static;
    class function CreateObjectList<T: class>(ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const values: array of T; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const values: IEnumerable<T>; ownsObjects: Boolean = True): IList<T>; overload; static;

    class function CreateInterfaceList(elementType: PTypeInfo): IInterfaceList; overload; static;
    class function CreateInterfaceList<T: IInterface>: IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const values: array of T): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const values: IEnumerable<T>): IList<T>; overload; static;

    class function CreateStringList: IList<string>; overload; static;
    class function CreateStringList(const comparer: IComparer<string>): IList<string>; overload; static;
    class function CreateStringList(const comparer: TComparison<string>): IList<string>; overload; static;
    class function CreateStringList(const values: array of string): IList<string>; overload; static;
    class function CreateStringList(const values: IEnumerable<string>): IList<string>; overload; static;

    class function CreateSortedList<T>: IList<T>; overload; static;
    class function CreateSortedList<T>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateSortedList<T>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateSortedList<T>(const values: array of T): IList<T>; overload; static;
    class function CreateSortedList<T>(const values: IEnumerable<T>): IList<T>; overload; static;
    class function CreateSortedList<T: class>(ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateSortedList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateSortedList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(const values: array of T; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedObjectList<T: class>(const values: IEnumerable<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>: IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>(const values: array of T): IList<T>; overload; static;
    class function CreateSortedInterfaceList<T: IInterface>(const values: IEnumerable<T>): IList<T>; overload; static;

    class function CreateObservableList<T: class>(ownsObjects: Boolean = True): IList<T>; static;
    class function CreateObservableInterfaceList<T: IInterface>: IList<T>; static;

    class function CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; ownerships: TDictionaryOwnerships = []): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IDictionary<TKey, TValue>; overload; static;

    class function CreateMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;

    class function CreateHashMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateHashMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateHashMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;

    class function CreateTreeMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateTreeMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;
    class function CreateTreeMultiMap<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; const valueComparer: IComparer<TValue>; ownerships: TDictionaryOwnerships = []): IMultiMap<TKey, TValue>; overload; static;

    class function CreateBidiDictionary<TKey, TValue>: IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(capacity: Integer; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(capacity: Integer; const keyComparer: IEqualityComparer<TKey>; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;
    class function CreateBidiDictionary<TKey, TValue>(capacity: Integer; const keyComparer: IEqualityComparer<TKey>; const valueComparer: IEqualityComparer<TValue>; ownerships: TDictionaryOwnerships = []): IBidiDictionary<TKey, TValue>; overload; static;

    class function CreateStack<T>: IStack<T>; overload; static;
    class function CreateStack<T: class>(ownsObjects: Boolean): IStack<T>; overload; static;
    class function CreateStack<T>(const values: array of T): IStack<T>; overload; static;
    class function CreateStack<T>(const values: IEnumerable<T>): IStack<T>; overload; static;

    class function CreateBoundedStack<T>(size: Integer): IStack<T>; overload; static;
    class function CreateBoundedStack<T: class>(size: Integer; ownsObjects: Boolean): IStack<T>; overload; static;

    class function CreateQueue<T>: IQueue<T>; overload; static;
    class function CreateQueue<T: class>(ownsObjects: Boolean): IQueue<T>; overload; static;
    class function CreateQueue<T>(const values: array of T): IQueue<T>; overload; static;
    class function CreateQueue<T>(const values: IEnumerable<T>): IQueue<T>; overload; static;

    class function CreateBoundedQueue<T>(size: Integer): IQueue<T>; overload; static;
    class function CreateBoundedQueue<T: class>(size: Integer; ownsObjects: Boolean): IQueue<T>; overload; static;

    class function CreateEvictingQueue<T>(size: Integer): IQueue<T>; overload; static;
    class function CreateEvictingQueue<T: class>(size: Integer; ownsObjects: Boolean): IQueue<T>; overload; static;

    class function CreateDeque<T>: IDeque<T>; overload; static;
    class function CreateDeque<T: class>(ownsObjects: Boolean): IDeque<T>; overload; static;
    class function CreateDeque<T>(const values: array of T): IDeque<T>; overload; static;
    class function CreateDeque<T>(const values: IEnumerable<T>): IDeque<T>; overload; static;

    class function CreateBoundedDeque<T>(size: Integer): IDeque<T>; overload; static;
    class function CreateBoundedDeque<T: class>(size: Integer; ownsObjects: Boolean): IDeque<T>; overload; static;

    class function CreateEvictingDeque<T>(size: Integer): IDeque<T>; overload; static;
    class function CreateEvictingDeque<T: class>(size: Integer; ownsObjects: Boolean): IDeque<T>; overload; static;

    class function CreateSet<T>: ISet<T>; overload; static;
    class function CreateSet<T>(capacity: Integer): ISet<T>; overload; static;
    class function CreateSet<T>(const comparer: IEqualityComparer<T>): ISet<T>; overload; static;
    class function CreateSet<T>(capacity: Integer; const comparer: IEqualityComparer<T>): ISet<T>; overload; static;
    class function CreateSet<T>(const values: array of T): ISet<T>; overload; static;
    class function CreateSet<T>(const values: IEnumerable<T>): ISet<T>; overload; static;

    class function CreateMultiSet<T>: IMultiSet<T>; overload; static;
    class function CreateMultiSet<T>(const comparer: IEqualityComparer<T>): IMultiSet<T>; overload; static;
    class function CreateMultiSet<T>(const values: array of T): IMultiSet<T>; overload; static;
    class function CreateMultiSet<T>(const values: IEnumerable<T>): IMultiSet<T>; overload; static;

    class function CreateSortedSet<T>: ISet<T>; overload; static;
    class function CreateSortedSet<T>(const comparer: IComparer<T>): ISet<T>; overload; static;
    class function CreateSortedSet<T>(const values: array of T): ISet<T>; overload; static;
    class function CreateSortedSet<T>(const values: IEnumerable<T>): ISet<T>; overload; static;

    class function CreateSortedMultiSet<T>: IMultiSet<T>; overload; static;
    class function CreateSortedMultiSet<T>(const comparer: IComparer<T>): IMultiSet<T>; overload; static;
    class function CreateSortedMultiSet<T>(const values: array of T): IMultiSet<T>; overload; static;
    class function CreateSortedMultiSet<T>(const values: IEnumerable<T>): IMultiSet<T>; overload; static;

    class function CreateSortedDictionary<TKey, TValue>: IDictionary<TKey, TValue>; overload; static;
    class function CreateSortedDictionary<TKey, TValue>(const keyComparer: IComparer<TKey>): IDictionary<TKey, TValue>; overload; static;
    class function CreateSortedDictionary<TKey, TValue>(const valueComparer: IEqualityComparer<TValue>): IDictionary<TKey, TValue>; overload; static;
    class function CreateSortedDictionary<TKey, TValue>(const keyComparer: IComparer<TKey>; const valueComparer: IEqualityComparer<TValue>): IDictionary<TKey, TValue>; overload; static;
  end;

  TEnumerable = class
  protected
    class procedure InternalFrom_Object_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Object_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Interface_DynArray(source: Pointer; var result; elementType: PTypeInfo); static;
    class procedure InternalFrom_Interface_OpenArray(source: Pointer; count: Integer; var result; elementType: PTypeInfo); static;
  public
    class function CombinePredicates<T>(const predicate1, predicate2: Predicate<T>): Predicate<T>; static;

    /// <summary>
    ///   Returns the elements of the specified sequence or the type
    ///   parameter's default value in a singleton collection if the sequence
    ///   is empty.
    /// </summary>
    class function DefaultIfEmpty<T>(const source: IEnumerable<T>): IEnumerable<T>; overload; static;

    /// <summary>
    ///   Returns the elements of the specified sequence or the specified value
    ///   in a singleton collection if the sequence is empty.
    /// </summary>
    class function DefaultIfEmpty<T>(const source: IEnumerable<T>; const defaultValue: T): IEnumerable<T>; overload; static;

    /// <summary>
    ///   Returns an empty <see cref="IEnumerable&lt;T&gt;" /> that has the
    ///   specified type argument.
    /// </summary>
    /// <typeparam name="T">
    ///   The type to assign to the type parameter of the returned generic <see cref="IEnumerable&lt;T&gt;" />
    ///    .
    /// </typeparam>
    /// <returns>
    ///   An empty <see cref="IEnumerable&lt;T&gt;" /> whose type argument is <i>
    ///   T</i>.
    /// </returns>
    class function Empty<T>: IReadOnlyList<T>; static;

    class function From<T>(const source: array of T): IReadOnlyList<T>; overload; static;
    class function From<T>(const source: TArray<T>): IReadOnlyList<T>; overload; static;

    class function Min<T>(const source: IEnumerable<T>;
      const selector: Func<T, Integer>): Integer; overload; static;
    class function Max<T>(const source: IEnumerable<T>;
      const selector: Func<T, Integer>): Integer; overload; static;

    class function OfType<T, TResult>(const source: IEnumerable<T>): IEnumerable<TResult>; static;

    class function OrderBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T,TKey>): IEnumerable<T>; overload; static;
    class function OrderBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T,TKey>;
      const comparer: IComparer<TKey>): IEnumerable<T>; overload; static;

    class function OrderByDescending<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T,TKey>): IEnumerable<T>; overload; static;
    class function OrderByDescending<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T,TKey>;
      const comparer: IComparer<TKey>): IEnumerable<T>; overload; static;

    class function Range(start, count: Integer): IReadOnlyList<Integer>; static;

    class function Repeated<T>(const element: T; count: Integer): IEnumerable<T>; static;

    class function Select<T, TResult>(const source: IEnumerable<T>;
      const selector: Func<T, TResult>): IEnumerable<TResult>; overload; static;

    class function SelectMany<T, TResult>(const source: IEnumerable<T>;
      const selector: Func<T, IEnumerable<TResult>>): IEnumerable<TResult>; overload; static;

    class function ToDictionary<TSource, TKey>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>): IDictionary<TKey, TSource>; overload; static;
    class function ToDictionary<TSource, TKey>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TSource>; overload; static;
    class function ToDictionary<TSource, TKey, TElement>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>): IDictionary<TKey, TElement>; overload; static;
    class function ToDictionary<TSource, TKey, TElement>(const source: IEnumerable<TSource>;
      const keySelector: Func<TSource, TKey>;
      const elementSelector: Func<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TElement>; overload; static;

    class function ToLookup<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>): ILookup<TKey, T>; overload; static;
    class function ToLookup<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const comparer: IEqualityComparer<TKey>): ILookup<TKey, T>; overload; static;
    class function ToLookup<T, TKey, TElement>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const elementSelector: Func<T, TElement>): ILookup<TKey, TElement>; overload; static;
    class function ToLookup<T, TKey, TElement>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const elementSelector: Func<T, TElement>;
      const comparer: IEqualityComparer<TKey>): ILookup<TKey, TElement>; overload; static;

    class function Distinct<T>(const source: IEnumerable<T>): IEnumerable<T>; overload; static;
    class function Distinct<T>(const source: IEnumerable<T>;
      const comparer: IEqualityComparer<T>): IEnumerable<T>; overload; static;

    class function DistinctBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>): IEnumerable<T>; overload; static;
    class function DistinctBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<T>; overload; static;

    class function &Except<T>(const first, second: IEnumerable<T>): IEnumerable<T>; overload; static;
    class function &Except<T>(const first, second: IEnumerable<T>;
      const comparer: IEqualityComparer<T>): IEnumerable<T>; overload; static;

    class function GroupBy<T, TKey>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>): IEnumerable<IGrouping<TKey,T>>; overload; static;
    class function GroupBy<T, TKey, TElement>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>;
      const elementSelector: Func<T, TElement>): IEnumerable<IGrouping<TKey,TElement>>; overload; static;
    class function GroupBy<T, TKey, TElement, TResult>(const source: IEnumerable<T>;
      const keySelector: Func<T, TKey>; const elementSelector: Func<T, TElement>;
      const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>): IEnumerable<TResult>; overload; static;

    class function Intersect<T>(const first, second: IEnumerable<T>): IEnumerable<T>; overload; static;
    class function Intersect<T>(const first, second: IEnumerable<T>;
      const comparer: IEqualityComparer<T>): IEnumerable<T>; overload; static;

    class function Union<T>(const first, second: IEnumerable<T>): IEnumerable<T>; overload; static;
    class function Union<T>(const first, second: IEnumerable<T>;
      const comparer: IEqualityComparer<T>): IEnumerable<T>; overload; static;
  end;

  TStringComparer = class(TCustomComparer<string>)
  private
    fLocaleOptions: TLocaleOptions;
    fIgnoreCase: Boolean;
    class var
      fOrdinal: TStringComparer;
      fOrdinalIgnoreCase: TStringComparer;
  protected
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const Left, Right: string): Boolean; override;
    function GetHashCode(const Value: string): Integer; override;
  public
    constructor Create(localeOptions: TLocaleOptions; ignoreCase: Boolean);
    class constructor Create;
    class destructor Destroy;

    class function Ordinal: TStringComparer;
    class function OrdinalIgnoreCase: TStringComparer;
  end;

  TInstanceComparer<T> = record
  public
    class function Default: IComparer<T>; static; inline;
  end;

  TKeyComparer<TKey> = class abstract(TRefCountedObject)
  private
    fKeyComparer: IComparer<TKey>;
  public
    constructor Create(const keyComparer: IComparer<TKey>);
  end;

  TPairByKeyComparer<TKey,TValue> = class(TKeyComparer<TKey>, IComparer<TPair<TKey,TValue>>)
  private
    function Compare(const left, right: TPair<TKey,TValue>): Integer;
  end;

  TIdentityFunction<T> = record
  private class var
    fInstance: Func<T, T>;
  public
    class constructor Create;
    class destructor Destroy;
    class property Instance: Func<T, T> read fInstance;
  end;

  TCollectionHelper = class helper for TCollection
  public
    function AsList: IList<TCollectionItem>; overload;
    function AsList<T: TCollectionItem>: IList<T>; overload;
  end;

  AutoInitAttribute = class(ManagedAttribute)
    constructor Create(ownsObjects: Boolean = True);
  end;

function GetElementType(typeInfo: PTypeInfo): PTypeInfo;
function _LookupVtableInfo(intf: TDefaultGenericInterface; info: PTypeInfo; size: Integer): Pointer;

implementation

uses
  Character,
{$IFDEF DELPHIXE8_UP}
  System.Hash,
{$ENDIF}
  Rtti,
  Spring.Collections.Base,
  Spring.Collections.Dictionaries,
  Spring.Collections.Extensions,
  Spring.Collections.Lists,
  Spring.Collections.LinkedLists,
  Spring.Collections.MultiMaps,
  Spring.Collections.MultiSets,
  Spring.Collections.Queues,
  Spring.Collections.Sets,
  Spring.Collections.Stacks,
  Spring.ResourceStrings;


function GetElementType(typeInfo: PTypeInfo): PTypeInfo;
var
  typeParams: TArray<string>;
  qualifiedName: string;
  item: TRttiType;
begin
  typeParams := GetGenericTypeParameters(typeInfo.TypeName);
  if Length(typeParams) <> 1 then
    Exit(nil);

  qualifiedName := typeParams[0];
  item := TType.Context.FindType(qualifiedName);
  if Assigned(item) then
    Result := item.Handle
  else
  begin
    for item in TType.Context.GetTypes do
      if SameText(item.Name, qualifiedName) then
        Exit(item.Handle);
    Result := nil;
  end;
end;


{$REGION 'Instance comparer'}

function Compare_Instance(Inst: Pointer; const Left, Right: TObject): Integer; //FI:O804
var
  comparable: IComparable;
begin
  if Supports(Left, IComparable, comparable) then
    Result := comparable.CompareTo(Right)
  else
    if NativeUInt(Left) < NativeUInt(Right) then
      Result := -1
    else if NativeUInt(Left) > NativeUInt(Right) then
      Result := 1
    else
      Result := 0;
end;

const
  InstanceComparer_VTable: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Instance
  );
  InstanceComparer: Pointer = @InstanceComparer_VTable;

function _LookupVtableInfo(intf: TDefaultGenericInterface; info: PTypeInfo; size: Integer): Pointer;
begin
  if (info <> nil) and (info.Kind = tkClass) and (intf = giComparer) then
    Result := @InstanceComparer
  else
    Result := Generics.Defaults._LookupVtableInfo(intf, info, size);
end;

{$ENDREGION}


{$REGION 'TPair<TKey, TValue>'}

constructor TPair<TKey, TValue>.Create(const key: TKey; const value: TValue);
begin
  Self.Key := key;
  Self.Value := value;
end;

{$ENDREGION}


{$REGION 'TCollections'}

class procedure TCollections.CreateDictionary_UInt8_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt8,UInt8>(Result) := TFoldedDictionary<UInt8,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt8_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt8,UInt16>(Result) := TFoldedDictionary<UInt8,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt8_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt8,UInt32>(Result) := TFoldedDictionary<UInt8,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt8_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt8,UInt64>(Result) := TFoldedDictionary<UInt8,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt8_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt8,IInterface>(Result) := TFoldedDictionary<UInt8,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt8_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt8,string>(Result) := TFoldedDictionary<UInt8,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt16_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt16,UInt8>(Result) := TFoldedDictionary<UInt16,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt16_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt16,UInt16>(Result) := TFoldedDictionary<UInt16,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt16_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt16,UInt32>(Result) := TFoldedDictionary<UInt16,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt16_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt16,UInt64>(Result) := TFoldedDictionary<UInt16,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt16_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt16,IInterface>(Result) := TFoldedDictionary<UInt16,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt16_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt16,string>(Result) := TFoldedDictionary<UInt16,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt32_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt32,UInt8>(Result) := TFoldedDictionary<UInt32,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt32_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt32,UInt16>(Result) := TFoldedDictionary<UInt32,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt32_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt32,UInt32>(Result) := TFoldedDictionary<UInt32,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt32_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt32,UInt64>(Result) := TFoldedDictionary<UInt32,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt32_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt32,IInterface>(Result) := TFoldedDictionary<UInt32,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt32_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt32,string>(Result) := TFoldedDictionary<UInt32,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt64_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt64,UInt8>(Result) := TFoldedDictionary<UInt64,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt64_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt64,UInt16>(Result) := TFoldedDictionary<UInt64,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt64_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt64,UInt32>(Result) := TFoldedDictionary<UInt64,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt64_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt64,UInt64>(Result) := TFoldedDictionary<UInt64,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt64_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt64,IInterface>(Result) := TFoldedDictionary<UInt64,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_UInt64_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<UInt64,string>(Result) := TFoldedDictionary<UInt64,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<IInterface,UInt8>(Result) := TFoldedDictionary<IInterface,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<IInterface,UInt16>(Result) := TFoldedDictionary<IInterface,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<IInterface,UInt32>(Result) := TFoldedDictionary<IInterface,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<IInterface,UInt64>(Result) := TFoldedDictionary<IInterface,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<IInterface,IInterface>(Result) := TFoldedDictionary<IInterface,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_Interface_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<IInterface,string>(Result) := TFoldedDictionary<IInterface,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<string,UInt8>(Result) := TFoldedDictionary<string,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<string,UInt16>(Result) := TFoldedDictionary<string,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<string,UInt32>(Result) := TFoldedDictionary<string,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<string,UInt64>(Result) := TFoldedDictionary<string,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<string,IInterface>(Result) := TFoldedDictionary<string,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateDictionary_String_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IDictionary<string,string>(Result) := TFoldedDictionary<string,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt8_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt8,UInt8>(Result) := TFoldedBidiDictionary<UInt8,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt8_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt8,UInt16>(Result) := TFoldedBidiDictionary<UInt8,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt8_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt8,UInt32>(Result) := TFoldedBidiDictionary<UInt8,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt8_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt8,UInt64>(Result) := TFoldedBidiDictionary<UInt8,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt8_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt8,IInterface>(Result) := TFoldedBidiDictionary<UInt8,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt8_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt8,string>(Result) := TFoldedBidiDictionary<UInt8,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt8>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt16_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt16,UInt8>(Result) := TFoldedBidiDictionary<UInt16,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt16_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt16,UInt16>(Result) := TFoldedBidiDictionary<UInt16,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt16_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt16,UInt32>(Result) := TFoldedBidiDictionary<UInt16,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt16_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt16,UInt64>(Result) := TFoldedBidiDictionary<UInt16,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt16_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt16,IInterface>(Result) := TFoldedBidiDictionary<UInt16,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt16_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt16,string>(Result) := TFoldedBidiDictionary<UInt16,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt16>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt32_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt32,UInt8>(Result) := TFoldedBidiDictionary<UInt32,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt32_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt32,UInt16>(Result) := TFoldedBidiDictionary<UInt32,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt32_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt32,UInt32>(Result) := TFoldedBidiDictionary<UInt32,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt32_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt32,UInt64>(Result) := TFoldedBidiDictionary<UInt32,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt32_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt32,IInterface>(Result) := TFoldedBidiDictionary<UInt32,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt32_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt32,string>(Result) := TFoldedBidiDictionary<UInt32,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt32>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt64_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt64,UInt8>(Result) := TFoldedBidiDictionary<UInt64,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt64_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt64,UInt16>(Result) := TFoldedBidiDictionary<UInt64,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt64_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt64,UInt32>(Result) := TFoldedBidiDictionary<UInt64,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt64_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt64,UInt64>(Result) := TFoldedBidiDictionary<UInt64,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt64_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt64,IInterface>(Result) := TFoldedBidiDictionary<UInt64,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_UInt64_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<UInt64,string>(Result) := TFoldedBidiDictionary<UInt64,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<UInt64>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,UInt8>(Result) := TFoldedBidiDictionary<IInterface,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,UInt16>(Result) := TFoldedBidiDictionary<IInterface,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,UInt32>(Result) := TFoldedBidiDictionary<IInterface,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,UInt64>(Result) := TFoldedBidiDictionary<IInterface,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,IInterface>(Result) := TFoldedBidiDictionary<IInterface,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_Interface_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<IInterface,string>(Result) := TFoldedBidiDictionary<IInterface,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<IInterface>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_UInt8(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,UInt8>(Result) := TFoldedBidiDictionary<string,UInt8>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<UInt8>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_UInt16(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,UInt16>(Result) := TFoldedBidiDictionary<string,UInt16>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<UInt16>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_UInt32(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,UInt32>(Result) := TFoldedBidiDictionary<string,UInt32>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<UInt32>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_UInt64(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,UInt64>(Result) := TFoldedBidiDictionary<string,UInt64>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<UInt64>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_Interface(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,IInterface>(Result) := TFoldedBidiDictionary<string,IInterface>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<IInterface>(valueComparer), ownerships);
end;

class procedure TCollections.CreateBidiDictionary_String_String(capacity: Integer;
  keyComparer, valueComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IBidiDictionary<string,string>(Result) := TFoldedBidiDictionary<string,string>.Create(keyType, valueType, elementType, capacity, IEqualityComparer<string>(keyComparer), IEqualityComparer<string>(valueComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt8_UInt8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt8,UInt8>(Result) := TFoldedListMultiMap<UInt8,UInt8>.Create(keyType, valueType, elementType, IEqualityComparer<UInt8>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt8_UInt16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt8,UInt16>(Result) := TFoldedListMultiMap<UInt8,UInt16>.Create(keyType, valueType, elementType, IEqualityComparer<UInt8>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt8_UInt32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt8,UInt32>(Result) := TFoldedListMultiMap<UInt8,UInt32>.Create(keyType, valueType, elementType, IEqualityComparer<UInt8>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt8_UInt64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt8,UInt64>(Result) := TFoldedListMultiMap<UInt8,UInt64>.Create(keyType, valueType, elementType, IEqualityComparer<UInt8>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt8_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt8,IInterface>(Result) := TFoldedListMultiMap<UInt8,IInterface>.Create(keyType, valueType, elementType, IEqualityComparer<UInt8>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt8_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt8,string>(Result) := TFoldedListMultiMap<UInt8,string>.Create(keyType, valueType, elementType, IEqualityComparer<UInt8>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt16_UInt8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt16,UInt8>(Result) := TFoldedListMultiMap<UInt16,UInt8>.Create(keyType, valueType, elementType, IEqualityComparer<UInt16>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt16_UInt16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt16,UInt16>(Result) := TFoldedListMultiMap<UInt16,UInt16>.Create(keyType, valueType, elementType, IEqualityComparer<UInt16>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt16_UInt32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt16,UInt32>(Result) := TFoldedListMultiMap<UInt16,UInt32>.Create(keyType, valueType, elementType, IEqualityComparer<UInt16>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt16_UInt64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt16,UInt64>(Result) := TFoldedListMultiMap<UInt16,UInt64>.Create(keyType, valueType, elementType, IEqualityComparer<UInt16>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt16_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt16,IInterface>(Result) := TFoldedListMultiMap<UInt16,IInterface>.Create(keyType, valueType, elementType, IEqualityComparer<UInt16>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt16_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt16,string>(Result) := TFoldedListMultiMap<UInt16,string>.Create(keyType, valueType, elementType, IEqualityComparer<UInt16>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt32_UInt8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt32,UInt8>(Result) := TFoldedListMultiMap<UInt32,UInt8>.Create(keyType, valueType, elementType, IEqualityComparer<UInt32>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt32_UInt16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt32,UInt16>(Result) := TFoldedListMultiMap<UInt32,UInt16>.Create(keyType, valueType, elementType, IEqualityComparer<UInt32>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt32_UInt32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt32,UInt32>(Result) := TFoldedListMultiMap<UInt32,UInt32>.Create(keyType, valueType, elementType, IEqualityComparer<UInt32>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt32_UInt64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt32,UInt64>(Result) := TFoldedListMultiMap<UInt32,UInt64>.Create(keyType, valueType, elementType, IEqualityComparer<UInt32>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt32_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt32,IInterface>(Result) := TFoldedListMultiMap<UInt32,IInterface>.Create(keyType, valueType, elementType, IEqualityComparer<UInt32>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt32_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt32,string>(Result) := TFoldedListMultiMap<UInt32,string>.Create(keyType, valueType, elementType, IEqualityComparer<UInt32>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt64_UInt8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt64,UInt8>(Result) := TFoldedListMultiMap<UInt64,UInt8>.Create(keyType, valueType, elementType, IEqualityComparer<UInt64>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt64_UInt16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt64,UInt16>(Result) := TFoldedListMultiMap<UInt64,UInt16>.Create(keyType, valueType, elementType, IEqualityComparer<UInt64>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt64_UInt32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt64,UInt32>(Result) := TFoldedListMultiMap<UInt64,UInt32>.Create(keyType, valueType, elementType, IEqualityComparer<UInt64>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt64_UInt64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt64,UInt64>(Result) := TFoldedListMultiMap<UInt64,UInt64>.Create(keyType, valueType, elementType, IEqualityComparer<UInt64>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt64_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt64,IInterface>(Result) := TFoldedListMultiMap<UInt64,IInterface>.Create(keyType, valueType, elementType, IEqualityComparer<UInt64>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_UInt64_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<UInt64,string>(Result) := TFoldedListMultiMap<UInt64,string>.Create(keyType, valueType, elementType, IEqualityComparer<UInt64>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_UInt8(keyComparer: Pointer;
  ownerships: TDictionaryOwnerships; var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,UInt8>(Result) := TFoldedListMultiMap<IInterface,UInt8>.Create(keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_UInt16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType,valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,UInt16>(Result) := TFoldedListMultiMap<IInterface,UInt16>.Create(keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_UInt32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,UInt32>(Result) := TFoldedListMultiMap<IInterface,UInt32>.Create(keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_UInt64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships; var result; keyType,
  valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,UInt64>(Result) := TFoldedListMultiMap<IInterface,UInt64>.Create(keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,IInterface>(Result) := TFoldedListMultiMap<IInterface,IInterface>.Create(keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_Interface_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<IInterface,string>(Result) := TFoldedListMultiMap<IInterface,string>.Create(keyType, valueType, elementType, IEqualityComparer<IInterface>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_UInt8(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,UInt8>(Result) := TFoldedListMultiMap<string,UInt8>.Create(keyType, valueType, elementType, IEqualityComparer<string>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_UInt16(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,UInt16>(Result) := TFoldedListMultiMap<string,UInt16>.Create(keyType, valueType, elementType, IEqualityComparer<string>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_UInt32(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,UInt32>(Result) := TFoldedListMultiMap<string,UInt32>.Create(keyType, valueType, elementType, IEqualityComparer<string>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_UInt64(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,UInt64>(Result) := TFoldedListMultiMap<string,UInt64>.Create(keyType, valueType, elementType, IEqualityComparer<string>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_Interface(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships;
  var result; keyType, valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,IInterface>(Result) := TFoldedListMultiMap<string,IInterface>.Create(keyType, valueType, elementType, IEqualityComparer<string>(keyComparer), ownerships);
end;

class procedure TCollections.CreateListMultiMap_String_String(
  keyComparer: Pointer; ownerships: TDictionaryOwnerships; var result; keyType,
  valueType, elementType: PTypeInfo);
begin
  IMultiMap<string,string>(Result) := TFoldedListMultiMap<string,string>.Create(keyType, valueType, elementType, IEqualityComparer<string>(keyComparer), ownerships);
end;

class procedure TCollections.CreateList_UInt8(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<UInt8>(result) := TFoldedList<UInt8>.Create(elementType, IComparer<UInt8>(comparer));
end;

class procedure TCollections.CreateList_UInt16(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<UInt16>(result) := TFoldedList<UInt16>.Create(elementType, IComparer<UInt16>(comparer));
end;

class procedure TCollections.CreateList_UInt32(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<UInt32>(result) := TFoldedList<UInt32>.Create(elementType, IComparer<UInt32>(comparer));
end;

class procedure TCollections.CreateList_UInt64(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<UInt64>(result) := TFoldedList<UInt64>.Create(elementType, IComparer<UInt64>(comparer));
end;

class procedure TCollections.CreateList_Method(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<TMethodPointer>(Result) := TFoldedList<TMethodPointer>.Create(elementType, IComparer<TMethodPointer>(comparer));
end;

class procedure TCollections.CreateList_Interface(var result; elementType: Pointer);
begin
  IList<IInterface>(Result) := TFoldedList<IInterface>.Create(elementType, nil);
end;

class procedure TCollections.CreateList_Interface(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<IInterface>(Result) := TFoldedList<IInterface>.Create(elementType, IComparer<IInterface>(comparer));
end;

class procedure TCollections.CreateList_Object(var result; elementType: Pointer);
begin
  IList<TObject>(Result) := TFoldedList<TObject>.Create(elementType, nil, True);
end;

class procedure TCollections.CreateList_Object(ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IList<TObject>(Result) := TFoldedList<TObject>.Create(elementType, nil, ownsObjects);
end;

class procedure TCollections.CreateList_Object(comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IList<TObject>(Result) := TFoldedList<TObject>.Create(elementType, IComparer<TObject>(comparer), ownsObjects);
end;

class procedure TCollections.CreateList_String(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<string>(Result) := TFoldedList<string>.Create(elementType, IComparer<string>(comparer));
end;

class procedure TCollections.CreateSortedList_UInt8(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<UInt8>(result) := TFoldedSortedList<UInt8>.Create(elementType, IComparer<UInt8>(comparer));
end;

class procedure TCollections.CreateSortedList_UInt16(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<UInt16>(result) := TFoldedSortedList<UInt16>.Create(elementType, IComparer<UInt16>(comparer));
end;

class procedure TCollections.CreateSortedList_UInt32(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<UInt32>(result) := TFoldedSortedList<UInt32>.Create(elementType, IComparer<UInt32>(comparer));
end;

class procedure TCollections.CreateSortedList_UInt64(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<UInt64>(result) := TFoldedSortedList<UInt64>.Create(elementType, IComparer<UInt64>(comparer));
end;

class procedure TCollections.CreateSortedList_Interface(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<IInterface>(result) := TFoldedSortedList<IInterface>.Create(elementType, IComparer<IInterface>(comparer));
end;

class procedure TCollections.CreateSortedList_Object(comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IList<TObject>(result) := TFoldedSortedList<TObject>.Create(elementType, IComparer<TObject>(comparer), ownsObjects);
end;

class procedure TCollections.CreateSortedList_String(comparer: Pointer; var result; elementType: Pointer);
begin
  IList<string>(result) := TFoldedSortedList<string>.Create(elementType, IComparer<string>(comparer));
end;

class procedure TCollections.CreateHashSet_UInt8(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  ISet<UInt8>(result) := TFoldedHashSet<UInt8>.Create(elementType, capacity, IEqualityComparer<UInt8>(comparer));
end;

class procedure TCollections.CreateHashSet_UInt16(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  ISet<UInt16>(result) := TFoldedHashSet<UInt16>.Create(elementType, capacity, IEqualityComparer<UInt16>(comparer));
end;

class procedure TCollections.CreateHashSet_UInt32(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  ISet<UInt32>(result) := TFoldedHashSet<UInt32>.Create(elementType, capacity, IEqualityComparer<UInt32>(comparer));
end;

class procedure TCollections.CreateHashSet_UInt64(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  ISet<UInt64>(result) := TFoldedHashSet<UInt64>.Create(elementType, capacity, IEqualityComparer<UInt64>(comparer));
end;

class procedure TCollections.CreateHashSet_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  ISet<IInterface>(result) := TFoldedHashSet<IInterface>.Create(elementType, capacity, IEqualityComparer<IInterface>(comparer));
end;

class procedure TCollections.CreateHashSet_Object(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  ISet<TObject>(result) := TFoldedHashSet<TObject>.Create(elementType, capacity, IEqualityComparer<TObject>(comparer));
end;

class procedure TCollections.CreateHashSet_String(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  ISet<string>(result) := TFoldedHashSet<string>.Create(elementType, capacity, IEqualityComparer<string>(comparer));
end;

class procedure TCollections.CreateQueue_Object(capacity: Integer; comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IQueue<TObject>(result) := TFoldedQueue<TObject>.Create(elementType, IComparer<TObject>(comparer), ownsObjects);
end;

class procedure TCollections.CreateQueue_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IQueue<IInterface>(result) := TFoldedQueue<IInterface>.Create(elementType, IComparer<IInterface>(comparer));
end;

class procedure TCollections.CreateStack_Interface(capacity: Integer; comparer: Pointer; var result; elementType: Pointer);
begin
  IStack<IInterface>(result) := TFoldedStack<IInterface>.Create(elementType, IComparer<IInterface>(comparer));
end;

class procedure TCollections.CreateStack_Object(capacity: Integer; comparer: Pointer; ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IStack<TObject>(result) := TFoldedStack<TObject>.Create(elementType, IComparer<TObject>(comparer), ownsObjects);
end;

class procedure TCollections.CreateObservableList_Interface(var result; elementType: Pointer);
begin
  IList<IInterface>(result) := TObservableInterfaceList.Create(elementType);
end;

class procedure TCollections.CreateObservableList_Object(ownsObjects: Boolean; var result; elementType: Pointer);
begin
  IList<TObject>(result) := TObservableObjectList.Create(elementType, ownsObjects);
end;

class function TCollections.CreateList<T>: IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateList_Object(False, Result, TypeInfo(T));
    tkInterface: CreateList_Interface(Result, TypeInfo(T));
    tkUString: CreateList_String(nil, Result, TypeInfo(T));
    tkMethod: CreateList_Method(nil, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateList_UInt8(nil, Result, TypeInfo(T));
        2: CreateList_UInt16(nil, Result, TypeInfo(T));
        4: CreateList_UInt32(nil, Result, TypeInfo(T));
        8: CreateList_UInt64(nil, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<T>.Create;
  end;
end;

class function TCollections.CreateList<T>(const comparer: IComparer<T>): IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateList_Object(Pointer(comparer), False, Result, TypeInfo(T));
    tkInterface: CreateList_Interface(Pointer(comparer), Result, TypeInfo(T));
    tkUString: CreateList_String(Pointer(comparer), Result, TypeInfo(T));
    tkMethod: CreateList_Method(Pointer(comparer), Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateList_UInt8(Pointer(comparer), Result, TypeInfo(T));
        2: CreateList_UInt16(Pointer(comparer), Result, TypeInfo(T));
        4: CreateList_UInt32(Pointer(comparer), Result, TypeInfo(T));
        8: CreateList_UInt64(Pointer(comparer), Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<T>.Create(comparer);
  end;
end;

class function TCollections.CreateList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateList_Object(PPointer(@comparer)^, False, Result, TypeInfo(T));
    tkInterface: CreateList_Interface(PPointer(@comparer)^, Result, TypeInfo(T));
    tkUString: CreateList_String(PPointer(@comparer)^, Result, TypeInfo(T));
    tkMethod: CreateList_Method(PPointer(@comparer)^, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateList_UInt8(PPointer(@comparer)^, Result, TypeInfo(T));
        2: CreateList_UInt16(PPointer(@comparer)^, Result, TypeInfo(T));
        4: CreateList_UInt32(PPointer(@comparer)^, Result, TypeInfo(T));
        8: CreateList_UInt64(PPointer(@comparer)^, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TList<T>.Create(IComparer<T>(PPointer(@comparer)^));
  end;
end;

class function TCollections.CreateList<T>(const values: array of T): IList<T>;
begin
  Result := CreateList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateList<T>(const values: IEnumerable<T>): IList<T>;
begin
  Result := CreateList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateList<T>(ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateList<T>(const comparer: IComparer<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(Pointer(comparer), ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateList<T>(const comparer: TComparison<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(PPointer(@comparer)^, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList(elementType: PTypeInfo;
  ownsObjects: Boolean): IObjectList;
begin
  CreateList_Object(ownsObjects, Result, elementType)
end;

class function TCollections.CreateObjectList<T>: IList<T>;
begin
  CreateList_Object(Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList<T>(ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList<T>(const comparer: IComparer<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(Pointer(comparer), ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList<T>(const comparer: TComparison<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(PPointer(@comparer)^, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObjectList<T>(const values: array of T;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(ownsObjects, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateObjectList<T>(const values: IEnumerable<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateList_Object(ownsObjects, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateInterfaceList(
  elementType: PTypeInfo): IInterfaceList;
begin
  CreateList_Interface(Result, elementType);
end;

class function TCollections.CreateInterfaceList<T>: IList<T>;
begin
  CreateList_Interface(Result, TypeInfo(T));
end;

class function TCollections.CreateInterfaceList<T>(
  const comparer: IComparer<T>): IList<T>;
begin
  CreateList_Interface(Pointer(comparer), Result, TypeInfo(T));
end;

class function TCollections.CreateInterfaceList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
  CreateList_Interface(PPointer(@comparer)^, Result, TypeInfo(T));
end;

class function TCollections.CreateInterfaceList<T>(
  const values: array of T): IList<T>;
begin
  CreateList_Interface(Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateInterfaceList<T>(
  const values: IEnumerable<T>): IList<T>;
begin
  CreateList_Interface(Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateStringList: IList<string>;
begin
  CreateList_String(nil, Result, TypeInfo(string));
end;

class function TCollections.CreateStringList(
  const comparer: IComparer<string>): IList<string>;
begin
  CreateList_String(PPointer(@comparer)^, Result, TypeInfo(string));
end;

class function TCollections.CreateStringList(
  const comparer: TComparison<string>): IList<string>;
begin
  CreateList_String(PPointer(@comparer)^, Result, TypeInfo(string));
end;

class function TCollections.CreateStringList(
  const values: array of string): IList<string>;
begin
  CreateList_String(nil, Result, TypeInfo(string));
  Result.AddRange(values);
end;

class function TCollections.CreateStringList(
  const values: IEnumerable<string>): IList<string>;
begin
  CreateList_String(nil, Result, TypeInfo(string));
  Result.AddRange(values);
end;

class function TCollections.CreateObservableList<T>(
  ownsObjects: Boolean): IList<T>;
begin
  CreateObservableList_Object(ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateObservableInterfaceList<T>: IList<T>;
begin
  CreateObservableList_Interface(Result, TypeInfo(T));
end;

class function TCollections.CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt8_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt8_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt8_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt8_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt8_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt8_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt16_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt16_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt16_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt16_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt16_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt16_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt32_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt32_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt32_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt32_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt32_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt32_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt64_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt64_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt64_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt64_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt64_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt64_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_Interface_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Interface_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Interface_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Interface_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_Interface_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_Interface_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_String_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_String_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_String_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_String_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_String_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_String_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(0, nil, nil, []);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt8_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt8_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt8_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt8_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt8_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt8_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt16_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt16_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt16_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt16_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt16_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt16_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt32_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt32_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt32_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt32_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt32_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt32_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt64_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt64_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt64_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt64_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt64_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt64_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_Interface_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Interface_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Interface_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Interface_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_Interface_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_Interface_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_String_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_String_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_String_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_String_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_String_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_String_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(0, nil, nil, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(capacity: Integer;
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt8_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt8_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt8_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt8_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt8_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt8_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt16_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt16_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt16_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt16_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt16_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt16_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt32_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt32_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt32_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt32_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt32_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt32_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt64_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt64_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt64_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt64_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt64_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt64_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_Interface_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Interface_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Interface_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Interface_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_Interface_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_Interface_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_String_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_String_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_String_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_String_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_String_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_String_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(capacity, nil, nil, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt8_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt8_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt8_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt8_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt8_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt8_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt16_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt16_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt16_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt16_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt16_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt16_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt32_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt32_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt32_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt32_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt32_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt32_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt64_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt64_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt64_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt64_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt64_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt64_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_Interface_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Interface_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Interface_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Interface_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_Interface_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_Interface_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_String_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_String_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_String_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_String_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_String_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_String_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(0, keyComparer, nil, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt8_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt8_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt8_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt8_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt8_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt8_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt16_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt16_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt16_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt16_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt16_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt16_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt32_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt32_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt32_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt32_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt32_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt32_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt64_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt64_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt64_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt64_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt64_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt64_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_Interface_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Interface_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Interface_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Interface_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_Interface_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_Interface_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_String_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_String_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_String_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_String_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_String_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_String_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(0, keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt8_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt8_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt8_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt8_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt8_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt8_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt16_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt16_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt16_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt16_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt16_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt16_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt32_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt32_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt32_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt32_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt32_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt32_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt64_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt64_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt64_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt64_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt64_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt64_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_Interface_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Interface_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Interface_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Interface_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_Interface_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_Interface_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_String_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_String_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_String_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_String_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_String_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_String_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(capacity, keyComparer, nil, ownerships);
end;

class function TCollections.CreateDictionary<TKey, TValue>(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt8_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt8_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt8_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt8_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt8_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt8_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt16_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt16_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt16_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt16_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt16_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt16_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt32_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt32_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt32_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt32_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt32_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt32_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_UInt64_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_UInt64_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_UInt64_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_UInt64_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_UInt64_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_UInt64_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_Interface_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_Interface_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_Interface_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_Interface_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_Interface_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_Interface_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateDictionary_String_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateDictionary_String_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateDictionary_String_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateDictionary_String_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateDictionary_String_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateDictionary_String_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TDictionary<TKey, TValue>.Create(capacity, keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_UInt8_UInt8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_UInt8_UInt16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_UInt8_UInt32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_UInt8_UInt64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_UInt8_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_UInt8_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_UInt16_UInt8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_UInt16_UInt16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_UInt16_UInt32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_UInt16_UInt64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_UInt16_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_UInt16_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_UInt32_UInt8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_UInt32_UInt16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_UInt32_UInt32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_UInt32_UInt64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_UInt32_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_UInt32_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_UInt64_UInt8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_UInt64_UInt16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_UInt64_UInt32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_UInt64_UInt64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_UInt64_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_UInt64_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_Interface_UInt8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Interface_UInt16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Interface_UInt32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Interface_UInt64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_Interface_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_Interface_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_String_UInt8(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_String_UInt16(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_String_UInt32(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_String_UInt64(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_String_Interface(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_String_String(nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TListMultiMap<TKey, TValue>.Create(nil, ownerships);
end;

class function TCollections.CreateMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_UInt8_UInt8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_UInt8_UInt16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_UInt8_UInt32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_UInt8_UInt64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_UInt8_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_UInt8_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_UInt16_UInt8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_UInt16_UInt16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_UInt16_UInt32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_UInt16_UInt64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_UInt16_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_UInt16_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_UInt32_UInt8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_UInt32_UInt16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_UInt32_UInt32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_UInt32_UInt64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_UInt32_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_UInt32_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_UInt64_UInt8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_UInt64_UInt16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_UInt64_UInt32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_UInt64_UInt64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_UInt64_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_UInt64_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_Interface_UInt8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_Interface_UInt16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_Interface_UInt32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_Interface_UInt64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_Interface_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_Interface_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateListMultiMap_String_UInt8(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateListMultiMap_String_UInt16(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateListMultiMap_String_UInt32(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateListMultiMap_String_UInt64(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateListMultiMap_String_Interface(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateListMultiMap_String_String(Pointer(keyComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TListMultiMap<TKey, TValue>.Create(keyComparer, ownerships);
end;

class function TCollections.CreateHashMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := THashMultiMap<TKey, TValue>.Create(nil, nil, ownerships);
end;

class function TCollections.CreateHashMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := THashMultiMap<TKey, TValue>.Create(keyComparer, nil, ownerships);
end;

class function TCollections.CreateHashMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := THashMultiMap<TKey, TValue>.Create(keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateTreeMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TTreeMultiMap<TKey, TValue>.Create(nil, nil, ownerships);
end;

class function TCollections.CreateTreeMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TTreeMultiMap<TKey, TValue>.Create(keyComparer, nil, ownerships);
end;

class function TCollections.CreateTreeMultiMap<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IComparer<TValue>;
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TTreeMultiMap<TKey, TValue>.Create(keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateMultiSet<T>: IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create;
end;

class function TCollections.CreateMultiSet<T>(
  const comparer: IEqualityComparer<T>): IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create(comparer);
end;

class function TCollections.CreateMultiSet<T>(const values: array of T): IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create;
  Result.AddRange(values);
end;

class function TCollections.CreateMultiSet<T>(const values: IEnumerable<T>): IMultiSet<T>;
begin
  Result := THashMultiSet<T>.Create;
  Result.AddRange(values);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>: IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt8_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt8_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt8_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt8_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt8_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt8_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt16_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt16_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt16_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt16_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt16_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt16_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt32_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt32_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt32_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt32_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt32_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt32_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt64_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt64_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt64_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt64_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt64_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt64_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Interface_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Interface_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Interface_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Interface_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_Interface_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_Interface_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_String_UInt8(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_String_UInt16(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_String_UInt32(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_String_UInt64(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_String_Interface(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_String_String(0, nil, nil, [], result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(0, nil, nil, []);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt8_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt8_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt8_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt8_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt8_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt8_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt16_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt16_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt16_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt16_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt16_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt16_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt32_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt32_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt32_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt32_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt32_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt32_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt64_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt64_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt64_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt64_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt64_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt64_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Interface_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Interface_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Interface_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Interface_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_Interface_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_Interface_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_String_UInt8(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_String_UInt16(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_String_UInt32(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_String_UInt64(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_String_Interface(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_String_String(0, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(0, nil, nil, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(capacity: Integer;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt8_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt8_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt8_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt8_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt8_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt8_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt16_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt16_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt16_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt16_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt16_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt16_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt32_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt32_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt32_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt32_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt32_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt32_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt64_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt64_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt64_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt64_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt64_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt64_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Interface_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Interface_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Interface_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Interface_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_Interface_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_Interface_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_String_UInt8(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_String_UInt16(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_String_UInt32(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_String_UInt64(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_String_Interface(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_String_String(capacity, nil, nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(capacity, nil, nil, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt8_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt8_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt8_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt8_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt8_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt8_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt16_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt16_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt16_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt16_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt16_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt16_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt32_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt32_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt32_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt32_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt32_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt32_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt64_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt64_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt64_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt64_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt64_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt64_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Interface_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Interface_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Interface_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Interface_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_Interface_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_Interface_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_String_UInt8(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_String_UInt16(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_String_UInt32(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_String_UInt64(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_String_Interface(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_String_String(0, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(0, keyComparer, nil, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt8_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt8_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt8_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt8_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt8_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt8_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt16_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt16_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt16_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt16_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt16_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt16_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt32_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt32_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt32_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt32_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt32_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt32_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt64_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt64_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt64_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt64_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt64_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt64_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Interface_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Interface_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Interface_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Interface_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_Interface_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_Interface_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_String_UInt8(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_String_UInt16(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_String_UInt32(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_String_UInt64(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_String_Interface(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_String_String(0, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(0, keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt8_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt8_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt8_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt8_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt8_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt8_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt16_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt16_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt16_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt16_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt16_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt16_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt32_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt32_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt32_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt32_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt32_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt32_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt64_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt64_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt64_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt64_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt64_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt64_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Interface_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Interface_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Interface_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Interface_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_Interface_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_Interface_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_String_UInt8(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_String_UInt16(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_String_UInt32(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_String_UInt64(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_String_Interface(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_String_String(capacity, Pointer(keyComparer), nil, ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(capacity, keyComparer, nil, ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>(
  capacity: Integer;
  const keyComparer: IEqualityComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>;
  ownerships: TDictionaryOwnerships): IBidiDictionary<TKey, TValue>;
begin
{$IFDEF DELPHIXE7_UP}
  if (GetTypeKind(TKey) in FoldedTypeKinds) and (GetTypeKind(TValue) in FoldedTypeKinds) then
  if not IsManagedType(TKey) then
    case SizeOf(TKey) of
      1:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt8_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt8_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt8_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt8_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt8_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt8_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      2:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt16_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt16_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt16_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt16_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt16_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt16_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      4:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt32_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt32_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt32_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt32_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt32_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt32_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      8:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_UInt64_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_UInt64_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_UInt64_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_UInt64_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_UInt64_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_UInt64_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
    case GetTypeKind(TKey) of
      tkInterface:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_Interface_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_Interface_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_Interface_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_Interface_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_Interface_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_Interface_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
      tkUString:
        if not IsManagedType(TValue) then
          case SizeOf(TValue) of
            1: CreateBidiDictionary_String_UInt8(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            2: CreateBidiDictionary_String_UInt16(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            4: CreateBidiDictionary_String_UInt32(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            8: CreateBidiDictionary_String_UInt64(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end
        else
          case GetTypeKind(TValue) of
            tkInterface: CreateBidiDictionary_String_Interface(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
            tkUString: CreateBidiDictionary_String_String(capacity, Pointer(keyComparer), Pointer(valueComparer), ownerships, result, TypeInfo(TKey), TypeInfo(TValue), TypeInfo(TPair<TKey,TValue>));
          end;
    end
  else
{$ENDIF}
  Result := TBidiDictionary<TKey, TValue>.Create(capacity, keyComparer, valueComparer, ownerships);
end;

class function TCollections.CreateStack<T>: IStack<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateStack_Object(0, nil, False, Result, TypeInfo(T));
    tkInterface: CreateStack_Interface(0, nil, Result, TypeInfo(T));
  else{$ELSE}begin{$ENDIF}
    Result := TStack<T>.Create;
  end;
end;

class function TCollections.CreateStack<T>(ownsObjects: Boolean): IStack<T>;
begin
  CreateStack_Object(0, nil, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateStack<T>(const values: array of T): IStack<T>;
begin
  Result := TStack<T>.Create(values);
end;

class function TCollections.CreateStack<T>(
  const values: IEnumerable<T>): IStack<T>;
begin
  Result := TStack<T>.Create(values);
end;

class function TCollections.CreateBoundedStack<T>(size: Integer): IStack<T>;
begin
  Result := TBoundedStack<T>.Create(size);
end;

class function TCollections.CreateBoundedStack<T>(size: Integer;
  ownsObjects: Boolean): IStack<T>;
begin
  Result := TBoundedStack<T>.Create(size);
end;

class function TCollections.CreateQueue<T>: IQueue<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateQueue_Object(0, nil, False, Result, TypeInfo(T));
    tkInterface: CreateQueue_Interface(0, nil, Result, TypeInfo(T));
  else{$ELSE}begin{$ENDIF}
    Result := TQueue<T>.Create;
  end;
end;

class function TCollections.CreateQueue<T>(ownsObjects: Boolean): IQueue<T>;
begin
  CreateQueue_Object(0, nil, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateQueue<T>(const values: array of T): IQueue<T>;
begin
  Result := TQueue<T>.Create(values);
end;

class function TCollections.CreateQueue<T>(const values: IEnumerable<T>): IQueue<T>;
begin
  Result := TQueue<T>.Create(values);
end;

class function TCollections.CreateBoundedQueue<T>(size: Integer): IQueue<T>;
begin
  Result := TBoundedQueue<T>.Create(size);
end;

class function TCollections.CreateBoundedQueue<T>(size: Integer;
  ownsObjects: Boolean): IQueue<T>;
begin
  Result := TBoundedQueue<T>.Create(size, ownsObjects);
end;

class function TCollections.CreateEvictingQueue<T>(size: Integer): IQueue<T>;
begin
  Result := TEvictingQueue<T>.Create(size);
end;

class function TCollections.CreateEvictingQueue<T>(size: Integer;
  ownsObjects: Boolean): IQueue<T>;
begin
  Result := TEvictingQueue<T>.Create(size, ownsObjects);
end;

class function TCollections.CreateDeque<T>: IDeque<T>;
begin
  Result := TDeque<T>.Create;
end;

class function TCollections.CreateDeque<T>(ownsObjects: Boolean): IDeque<T>;
begin
  Result := TDeque<T>.Create(ownsObjects);
end;

class function TCollections.CreateDeque<T>(const values: array of T): IDeque<T>;
begin
  Result := TDeque<T>.Create(values);
end;

class function TCollections.CreateDeque<T>(const values: IEnumerable<T>): IDeque<T>;
begin
  Result := TDeque<T>.Create(values);
end;

class function TCollections.CreateBoundedDeque<T>(size: Integer): IDeque<T>;
begin
  Result := TBoundedDeque<T>.Create(size);
end;

class function TCollections.CreateBoundedDeque<T>(size: Integer;
  ownsObjects: Boolean): IDeque<T>;
begin
  Result := TBoundedDeque<T>.Create(size, ownsObjects);
end;

class function TCollections.CreateEvictingDeque<T>(size: Integer): IDeque<T>;
begin
  Result := TEvictingDeque<T>.Create(size);
end;

class function TCollections.CreateEvictingDeque<T>(size: Integer;
  ownsObjects: Boolean): IDeque<T>;
begin
  Result := TEvictingDeque<T>.Create(size, ownsObjects);
end;

class function TCollections.CreateSet<T>: ISet<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateHashSet_Object(0, nil, Result, TypeInfo(T));
    tkInterface: CreateHashSet_Interface(0, nil, Result, TypeInfo(T));
    tkUString: CreateHashSet_String(0, nil, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateHashSet_UInt8(0, nil, Result, TypeInfo(T));
        2: CreateHashSet_UInt16(0, nil, Result, TypeInfo(T));
        4: CreateHashSet_UInt32(0, nil, Result, TypeInfo(T));
        8: CreateHashSet_UInt64(0, nil, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := THashSet<T>.Create;
  end;
end;

class function TCollections.CreateSet<T>(capacity: Integer): ISet<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateHashSet_Object(capacity, nil, Result, TypeInfo(T));
    tkInterface: CreateHashSet_Interface(capacity, nil, Result, TypeInfo(T));
    tkUString: CreateHashSet_String(capacity, nil, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateHashSet_UInt8(capacity, nil, Result, TypeInfo(T));
        2: CreateHashSet_UInt16(capacity, nil, Result, TypeInfo(T));
        4: CreateHashSet_UInt32(capacity, nil, Result, TypeInfo(T));
        8: CreateHashSet_UInt64(capacity, nil, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := THashSet<T>.Create(capacity);
  end;
end;

class function TCollections.CreateSet<T>(
  const comparer: IEqualityComparer<T>): ISet<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateHashSet_Object(0, Pointer(comparer), Result, TypeInfo(T));
    tkInterface: CreateHashSet_Interface(0, Pointer(comparer), Result, TypeInfo(T));
    tkUString: CreateHashSet_String(0, Pointer(comparer), Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateHashSet_UInt8(0, Pointer(comparer), Result, TypeInfo(T));
        2: CreateHashSet_UInt16(0, Pointer(comparer), Result, TypeInfo(T));
        4: CreateHashSet_UInt32(0, Pointer(comparer), Result, TypeInfo(T));
        8: CreateHashSet_UInt64(0, Pointer(comparer), Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := THashSet<T>.Create(comparer);
  end;
end;

class function TCollections.CreateSet<T>(capacity: Integer;
  const comparer: IEqualityComparer<T>): ISet<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateHashSet_Object(capacity, Pointer(comparer), Result, TypeInfo(T));
    tkInterface: CreateHashSet_Interface(capacity, Pointer(comparer), Result, TypeInfo(T));
    tkUString: CreateHashSet_String(0, Pointer(comparer), Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateHashSet_UInt8(capacity, Pointer(comparer), Result, TypeInfo(T));
        2: CreateHashSet_UInt16(capacity, Pointer(comparer), Result, TypeInfo(T));
        4: CreateHashSet_UInt32(capacity, Pointer(comparer), Result, TypeInfo(T));
        8: CreateHashSet_UInt64(capacity, Pointer(comparer), Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := THashSet<T>.Create(capacity, comparer);
  end;
end;

class function TCollections.CreateSet<T>(const values: array of T): ISet<T>;
begin
  Result := CreateSet<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateSet<T>(const values: IEnumerable<T>): ISet<T>;
begin
  Result := CreateSet<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateSortedList<T>: IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateSortedList_Object(nil, False, Result, TypeInfo(T));
    tkInterface: CreateSortedList_Interface(nil, Result, TypeInfo(T));
    tkUString: CreateSortedList_String(nil, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateSortedList_UInt8(nil, Result, TypeInfo(T));
        2: CreateSortedList_UInt16(nil, Result, TypeInfo(T));
        4: CreateSortedList_UInt32(nil, Result, TypeInfo(T));
        8: CreateSortedList_UInt64(nil, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TSortedList<T>.Create;
  end;
end;

class function TCollections.CreateSortedList<T>(
  const comparer: IComparer<T>): IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateSortedList_Object(Pointer(comparer), False, Result, TypeInfo(T));
    tkInterface: CreateSortedList_Interface(Pointer(comparer), Result, TypeInfo(T));
    tkUString: CreateSortedList_String(Pointer(comparer), Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateSortedList_UInt8(Pointer(comparer), Result, TypeInfo(T));
        2: CreateSortedList_UInt16(Pointer(comparer), Result, TypeInfo(T));
        4: CreateSortedList_UInt32(Pointer(comparer), Result, TypeInfo(T));
        8: CreateSortedList_UInt64(Pointer(comparer), Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TSortedList<T>.Create(comparer);
  end;
end;

class function TCollections.CreateSortedList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: CreateSortedList_Object(PPointer(@comparer)^, False, Result, TypeInfo(T));
    tkInterface: CreateSortedList_Interface(PPointer(@comparer)^, Result, TypeInfo(T));
    tkUString: CreateSortedList_String(PPointer(@comparer)^, Result, TypeInfo(T));
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64, tkClassRef, tkPointer, tkProcedure:
      case SizeOf(T) of
        1: CreateSortedList_UInt8(PPointer(@comparer)^, Result, TypeInfo(T));
        2: CreateSortedList_UInt16(PPointer(@comparer)^, Result, TypeInfo(T));
        4: CreateSortedList_UInt32(PPointer(@comparer)^, Result, TypeInfo(T));
        8: CreateSortedList_UInt64(PPointer(@comparer)^, Result, TypeInfo(T));
      end;
  else{$ELSE}begin{$ENDIF}
    Result := TSortedList<T>.Create(IComparer<T>(PPointer(@comparer)^));
  end;
end;

class function TCollections.CreateSortedList<T>(
  const values: array of T): IList<T>;
begin
  Result := CreateSortedList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateSortedList<T>(
  const values: IEnumerable<T>): IList<T>;
begin
  Result := CreateSortedList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateSortedList<T>(
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(nil, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedList<T>(const comparer: IComparer<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(Pointer(comparer), ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedList<T>(const comparer: TComparison<T>;
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(PPointer(@comparer)^, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedObjectList<T>(
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(nil, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedObjectList<T>(
  const comparer: IComparer<T>; ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(Pointer(comparer), ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedObjectList<T>(
  const comparer: TComparison<T>; ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(PPointer(@comparer)^, ownsObjects, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedObjectList<T>(const values: array of T;
  ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(nil, ownsObjects, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateSortedObjectList<T>(
  const values: IEnumerable<T>; ownsObjects: Boolean): IList<T>;
begin
  CreateSortedList_Object(nil, ownsObjects, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateSortedInterfaceList<T>: IList<T>;
begin
  CreateSortedList_Interface(nil, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedInterfaceList<T>(
  const comparer: IComparer<T>): IList<T>;
begin
  CreateSortedList_Interface(Pointer(comparer), Result, TypeInfo(T));
end;

class function TCollections.CreateSortedInterfaceList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
  CreateSortedList_Interface(PPointer(@comparer)^, Result, TypeInfo(T));
end;

class function TCollections.CreateSortedInterfaceList<T>(
  const values: array of T): IList<T>;
begin
  CreateSortedList_Interface(nil, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateSortedInterfaceList<T>(
  const values: IEnumerable<T>): IList<T>;
begin
  CreateSortedList_Interface(nil, Result, TypeInfo(T));
  Result.AddRange(values);
end;

class function TCollections.CreateSortedSet<T>: ISet<T>;
begin
  Result := TSortedSet<T>.Create;
end;

class function TCollections.CreateSortedSet<T>(
  const comparer: IComparer<T>): ISet<T>;
begin
  Result := TSortedSet<T>.Create(comparer);
end;

class function TCollections.CreateSortedSet<T>(const values: array of T): ISet<T>;
begin
  Result := TSortedSet<T>.Create;
  Result.AddRange(values);
end;

class function TCollections.CreateSortedSet<T>(const values: IEnumerable<T>): ISet<T>;
begin
  Result := TSortedSet<T>.Create;
  Result.AddRange(values);
end;

class function TCollections.CreateSortedDictionary<TKey, TValue>: IDictionary<TKey, TValue>;
begin
  Result := TSortedDictionary<TKey, TValue>.Create;
end;

class function TCollections.CreateSortedDictionary<TKey, TValue>(
  const keyComparer: IComparer<TKey>): IDictionary<TKey, TValue>;
begin
  Result := TSortedDictionary<TKey, TValue>.Create(keyComparer, nil);
end;

class function TCollections.CreateSortedDictionary<TKey, TValue>(
  const valueComparer: IEqualityComparer<TValue>): IDictionary<TKey, TValue>;
begin
  Result := TSortedDictionary<TKey, TValue>.Create(nil, valueComparer);
end;

class function TCollections.CreateSortedDictionary<TKey, TValue>(
  const keyComparer: IComparer<TKey>;
  const valueComparer: IEqualityComparer<TValue>): IDictionary<TKey, TValue>;
begin
  Result := TSortedDictionary<TKey, TValue>.Create(keyComparer, valueComparer);
end;

class function TCollections.CreateSortedMultiSet<T>: IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create;
end;

class function TCollections.CreateSortedMultiSet<T>(
  const comparer: IComparer<T>): IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create(comparer);
end;

class function TCollections.CreateSortedMultiSet<T>(
  const values: IEnumerable<T>): IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create;
  Result.AddRange(values);
end;

class function TCollections.CreateSortedMultiSet<T>(
  const values: array of T): IMultiSet<T>;
begin
  Result := TTreeMultiSet<T>.Create;
  Result.AddRange(values);
end;

{$ENDREGION}


{$REGION 'TEnumerable'}

class function TEnumerable.CombinePredicates<T>(const predicate1,
  predicate2: Predicate<T>): Predicate<T>;
begin
  Result :=
    function(const x: T): Boolean
    begin
      Result := predicate1(x) and predicate2(x);
    end;
end;

class function TEnumerable.DefaultIfEmpty<T>(
  const source: IEnumerable<T>): IEnumerable<T>;
var
  defaultValue: T;
begin
  defaultValue := Default(T);
  Result := TDefaultIfEmptyIterator<T>.Create(source, defaultValue);
end;

class function TEnumerable.DefaultIfEmpty<T>(const source: IEnumerable<T>;
  const defaultValue: T): IEnumerable<T>;
begin
  Result := TDefaultIfEmptyIterator<T>.Create(source, defaultValue);
end;

class function TEnumerable.Distinct<T>(
  const source: IEnumerable<T>): IEnumerable<T>;
begin
  Result := TDistinctIterator<T>.Create(source, nil);
end;

class function TEnumerable.Distinct<T>(const source: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  Result := TDistinctIterator<T>.Create(source, comparer);
end;

class function TEnumerable.DistinctBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): IEnumerable<T>;
begin
  Result := TDistinctByIterator<T, TKey>.Create(source, keySelector, nil);
end;

class function TEnumerable.DistinctBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<T>;
begin
  Result := TDistinctByIterator<T, TKey>.Create(source, keySelector, comparer);
end;

class function TEnumerable.Empty<T>: IReadOnlyList<T>;
begin
  Result := TEmptyEnumerable<T>.Instance;
end;

class function TEnumerable.&Except<T>(const first,
  second: IEnumerable<T>): IEnumerable<T>;
begin
  Result := TExceptIterator<T>.Create(first, second);
end;

class function TEnumerable.&Except<T>(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  Result := TExceptIterator<T>.Create(first, second, comparer);
end;

class procedure TEnumerable.InternalFrom_Object_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<TObject>(Result) :=
    TObjectArrayIterator.Create(TArray<TObject>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_Object_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<TObject>(Result) :=
    TObjectArrayIterator.CreateFromArray(source, count, elementType);
end;

class procedure TEnumerable.InternalFrom_Interface_DynArray(source: Pointer;
  var result; elementType: PTypeInfo);
begin
  IReadOnlyList<IInterface>(Result) :=
    TInterfaceArrayIterator.Create(TArray<IInterface>(source), elementType);
end;

class procedure TEnumerable.InternalFrom_Interface_OpenArray(source: Pointer;
  count: Integer; var result; elementType: PTypeInfo);
begin
  IReadOnlyList<IInterface>(Result) :=
    TInterfaceArrayIterator.CreateFromArray(source, count, elementType);
end;

class function TEnumerable.From<T>(const source: array of T): IReadOnlyList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: InternalFrom_Object_OpenArray(@source, Length(source), Result, TypeInfo(T));
    tkInterface: InternalFrom_Interface_OpenArray(@source, Length(source), Result, TypeInfo(T));
  else{$ELSE}begin{$ENDIF}
    Result := TArrayIterator<T>.Create(source);
  end;
end;

class function TEnumerable.From<T>(const source: TArray<T>): IReadOnlyList<T>;
begin
{$IFDEF DELPHIXE7_UP}
  case GetTypeKind(T) of
    tkClass: InternalFrom_Object_DynArray(source, Result, TypeInfo(T));
    tkInterface: InternalFrom_Interface_DynArray(source, Result, TypeInfo(T));
  else{$ELSE}begin{$ENDIF}
    Result := TArrayIterator<T>.Create(source);
  end;
end;

class function TEnumerable.GroupBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): IEnumerable<IGrouping<TKey, T>>;
begin
  IEnumerable<IInterface>(Result) := TGroupedEnumerable<T, TKey, T>.Create(
    source, keySelector, function(const item: T): T begin Result := item end);
end;

class function TEnumerable.GroupBy<T, TKey, TElement>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>): IEnumerable<IGrouping<TKey, TElement>>;
begin
  IEnumerable<IInterface>(Result) := TGroupedEnumerable<T, TKey, TElement>.Create(
    source, keySelector, elementSelector);
end;

class function TEnumerable.GroupBy<T, TKey, TElement, TResult>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>;
  const resultSelector: Func<TKey, IEnumerable<TElement>, TResult>): IEnumerable<TResult>;
begin
  Result := TGroupedEnumerable<T, TKey, TElement, TResult>.Create(
    source, keySelector, elementSelector, resultSelector);
end;

class function TEnumerable.Intersect<T>(const first,
  second: IEnumerable<T>): IEnumerable<T>;
begin
  Result := TIntersectIterator<T>.Create(first, second);
end;

class function TEnumerable.Intersect<T>(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  Result := TIntersectIterator<T>.Create(first, second, comparer);
end;

class function TEnumerable.Max<T>(const source: IEnumerable<T>;
  const selector: Func<T, Integer>): Integer;
begin
  Result := Select<T, Integer>(source, selector).Max;
end;

class function TEnumerable.Min<T>(const source: IEnumerable<T>;
  const selector: Func<T, Integer>): Integer;
begin
  Result := Select<T, Integer>(source, selector).Min;
end;

class function TEnumerable.OfType<T, TResult>(
  const source: IEnumerable<T>): IEnumerable<TResult>;
begin
  Result := TOfTypeIterator<T, TResult>.Create(source);
end;

class function TEnumerable.OrderBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): IEnumerable<T>;
begin
  Result := TOrderedEnumerable<T,TKey>.Create(source, keySelector);
end;

class function TEnumerable.OrderBy<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>;
  const comparer: IComparer<TKey>): IEnumerable<T>;
begin
  Result := TOrderedEnumerable<T,TKey>.Create(source, keySelector, comparer);
end;

class function TEnumerable.OrderByDescending<T, TKey>(
  const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): IEnumerable<T>;
begin
  Result := TOrderedEnumerable<T,TKey>.Create(source, keySelector, nil, True);
end;

class function TEnumerable.OrderByDescending<T, TKey>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const comparer: IComparer<TKey>): IEnumerable<T>;
begin
  Result := TOrderedEnumerable<T,TKey>.Create(source, keySelector, comparer, True);
end;

class function TEnumerable.Range(start, count: Integer): IReadOnlyList<Integer>;
begin
  Result := TRangeIterator.Create(start, count);
end;

class function TEnumerable.Repeated<T>(const element: T;
  count: Integer): IEnumerable<T>;
begin
  Result := TRepeatIterator<T>.Create(element, count);
end;

class function TEnumerable.Select<T, TResult>(const source: IEnumerable<T>;
  const selector: Func<T, TResult>): IEnumerable<TResult>;
begin
  Result := TSelectIterator<T, TResult>.Create(source, selector);
end;

class function TEnumerable.SelectMany<T, TResult>(const source: IEnumerable<T>;
  const selector: Func<T, IEnumerable<TResult>>): IEnumerable<TResult>;
begin
  Result := TSelectManyIterator<T, TResult>.Create(source, selector);
end;

class function TEnumerable.ToDictionary<TSource, TKey>(
  const source: IEnumerable<TSource>;
  const keySelector: Func<TSource, TKey>): IDictionary<TKey, TSource>;
begin
  Result := ToDictionary<TSource, TKey, TSource>(source, keySelector,
    TIdentityFunction<TSource>.Instance, nil);
end;

class function TEnumerable.ToDictionary<TSource, TKey>(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TSource>;
begin
  Result := ToDictionary<TSource, TKey, TSource>(source, keySelector,
    TIdentityFunction<TSource>.Instance, comparer);
end;

class function TEnumerable.ToDictionary<TSource, TKey, TElement>(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>): IDictionary<TKey, TElement>;
begin
  Result := ToDictionary<TSource, TKey, TElement>(source, keySelector,
    elementSelector, nil);
end;

class function TEnumerable.ToDictionary<TSource, TKey, TElement>(
  const source: IEnumerable<TSource>; const keySelector: Func<TSource, TKey>;
  const elementSelector: Func<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TElement>;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
  Guard.CheckNotNull(Assigned(elementSelector), 'elementSelector');
  Result := TCollections.CreateDictionary<TKey, TElement>(comparer);
  for item in source do
    Result.Add(keySelector(item), elementSelector(item));
end;

class function TEnumerable.ToLookup<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>): ILookup<TKey, T>;
begin
  ILookupInternal<TKey, T>(Result) := TLookup<TKey, T>.Create<T>(source, keySelector,
    function(const x: T): T
    begin
      Result := x
    end);
end;

class function TEnumerable.ToLookup<T, TKey>(const source: IEnumerable<T>;
  const keySelector: Func<T, TKey>; const comparer: IEqualityComparer<TKey>): ILookup<TKey, T>;
begin
  ILookupInternal<TKey, T>(Result) := TLookup<TKey, T>.Create<T>(source, keySelector,
    function(const x: T): T
    begin
      Result := x
    end, comparer);
end;

class function TEnumerable.ToLookup<T, TKey, TElement>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>): ILookup<TKey, TElement>;
begin
  ILookupInternal<TKey, TElement>(Result) := TLookup<TKey, TElement>.Create<T>(
    source, keySelector, elementSelector);
end;

class function TEnumerable.ToLookup<T, TKey, TElement>(
  const source: IEnumerable<T>; const keySelector: Func<T, TKey>;
  const elementSelector: Func<T, TElement>;
  const comparer: IEqualityComparer<TKey>): ILookup<TKey, TElement>;
begin
  ILookupInternal<TKey, TElement>(Result) := TLookup<TKey, TElement>.Create<T>(
    source, keySelector, elementSelector, comparer);
end;

class function TEnumerable.Union<T>(const first, second: IEnumerable<T>; const comparer: IEqualityComparer<T>): IEnumerable<T>;
begin
  Result := TUnionIterator<T>.Create(first, second, comparer);
end;

class function TEnumerable.Union<T>(const first, second: IEnumerable<T>): IEnumerable<T>;
begin
  Result := TUnionIterator<T>.Create(first, second);
end;

{$ENDREGION}


{$REGION 'TStringComparer'}

constructor TStringComparer.Create(localeOptions: TLocaleOptions;
  ignoreCase: Boolean);
begin
  inherited Create;
  fLocaleOptions := localeOptions;
  fIgnoreCase := ignoreCase;
end;

class constructor TStringComparer.Create;
begin
  fOrdinal := TStringComparer.Create(loInvariantLocale, False);
  fOrdinalIgnoreCase := TStringComparer.Create(loInvariantLocale, True);
end;

class destructor TStringComparer.Destroy;
begin
  FreeAndNil(fOrdinal);
  FreeAndNil(fOrdinalIgnoreCase);
end;

function TStringComparer.Compare(const Left, Right: string): Integer;
var
  L, R: string;
begin
  if fIgnoreCase then
  begin
{$IFNDEF DELPHIXE4_UP}
    L := TCharacter.ToUpper(Left);
    R := TCharacter.ToUpper(Right);
{$ELSE}
    L := Char.ToUpper(Left);
    R := Char.ToUpper(Right);
{$ENDIF}
  end else
  begin
    L := Left;
    R := Right;
  end;

  Result := CompareStr(L, R, fLocaleOptions);
end;

function TStringComparer.Equals(const Left, Right: string): Boolean;
var
  L, R: string;
begin
  if fIgnoreCase then
  begin
{$IFNDEF DELPHIXE4_UP}
    L := TCharacter.ToUpper(Left);
    R := TCharacter.ToUpper(Right);
{$ELSE}
    L := Char.ToUpper(Left);
    R := Char.ToUpper(Right);
{$ENDIF}
  end else
  begin
    L := Left;
    R := Right;
  end;

  Result := SameStr(L, R, fLocaleOptions);
end;

function TStringComparer.GetHashCode(const Value: string): Integer;
var
  s: string;
begin
  if fIgnoreCase then
{$IFNDEF DELPHIXE4_UP}
    S := TCharacter.ToUpper(Value)
{$ELSE}
    S := Char.ToUpper(Value)
{$ENDIF}
  else
    S := Value;

{$IFDEF DELPHIXE8_UP}
  Result := THashBobJenkins.GetHashValue(S);
{$ELSE}
  Result := BobJenkinsHash(PChar(S)^, SizeOf(Char) * Length(S), 0);
{$ENDIF}
end;

class function TStringComparer.Ordinal: TStringComparer;
begin
  Result := fOrdinal;
end;

class function TStringComparer.OrdinalIgnoreCase: TStringComparer;
begin
  Result := fOrdinalIgnoreCase;
end;

{$ENDREGION}


{$REGION 'TInstanceComparer<T>'}

class function TInstanceComparer<T>.Default: IComparer<T>;
begin
  Result := IComparer<T>(_LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)));
end;

{$ENDREGION}


{$REGION 'TKeyComparer<TKey>'}

constructor TKeyComparer<TKey>.Create(const keyComparer: IComparer<TKey>);
begin
  if keyComparer = nil then
    fKeyComparer := IComparer<TKey>(_LookupVtableInfo(giComparer, TypeInfo(TKey), SizeOf(TKey)))
  else
    fKeyComparer := keyComparer;
end;

{$ENDREGION}


{$REGION 'TPairByKeyComparer<TKey,TValue>'}

function TPairByKeyComparer<TKey,TValue>.Compare(
  const left, right: TPair<TKey, TValue>): Integer;
begin
  Result := fKeyComparer.Compare(left.Key, right.Key);
end;

{$ENDREGION}


{$REGION 'TLinkedListNode<T>'}

constructor TLinkedListNode<T>.Create(const value: T);
begin
  inherited Create;
  fItem := value;
end;

function TLinkedListNode<T>.GetList: ILinkedList<T>;
begin
  Result := TLinkedList<T>(fList);
end;

function TLinkedListNode<T>.GetNext: TLinkedListNode<T>;
begin
  if Assigned(fNext) and (fNext <> TLinkedList<T>(fList).fHead) then
    Result := fNext
  else
    Result := nil;
end;

function TLinkedListNode<T>.GetPrevious: TLinkedListNode<T>;
begin
  if Assigned(fPrev) and (Self <> TLinkedList<T>(fList).fHead) then
    Result := fPrev
  else
    Result := nil;
end;

{$ENDREGION}


{$REGION 'TKeyValuePair<TKey, TValue>'}

//constructor TKeyValuePair<TKey, TValue>.Create(const key: TKey;
//  const value: TValue);
//begin
//  fKey := key;
//  fValue := value;
//end;

{$ENDREGION}


{$REGION 'TCollectionHelper'}

function TCollectionHelper.AsList: IList<TCollectionItem>;
begin
  Result := TCollectionList<TCollectionItem>.Create(Self);
end;

function TCollectionHelper.AsList<T>: IList<T>;
begin
  Result := TCollectionList<T>.Create(Self);
end;

{$ENDREGION}


{$REGION 'AutoInitAttribute'}

function InitElementType(fieldType: PTypeInfo): PTypeInfo;
begin
  Assert(fieldType.Kind = tkInterface);
  Assert(fieldType.TypeData.GUID = IList<TObject>);
  Result := GetElementType(fieldType);
  Assert(Result.Kind in [tkClass, tkInterface]);
end;

constructor AutoInitAttribute.Create(ownsObjects: Boolean);
var
  elementType: PTypeInfo;
begin
  elementType := nil;
  inherited Create(
    function(fieldType: PTypeInfo): Pointer
    begin
      if elementType = nil then
        elementType := InitElementType(fieldType);

      Result := nil;
      case elementType.Kind of
        tkClass:     IInterface(Result) := TCollections.CreateObjectList(elementType, ownsObjects);
        tkInterface: IInterface(Result) := TCollections.CreateInterfaceList(elementType);
      end;
    end);
end;

{$ENDREGION}


{$REGION 'TIdentityFunction<T>'}

class constructor TIdentityFunction<T>.Create;
begin
  fInstance :=
    function(const x: T): T
    begin
      Result := x
    end;
end;

class destructor TIdentityFunction<T>.Destroy;
begin
  fInstance := nil;
end;

{$ENDREGION}


end.
