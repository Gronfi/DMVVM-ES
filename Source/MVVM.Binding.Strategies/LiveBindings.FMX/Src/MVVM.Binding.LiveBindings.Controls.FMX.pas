unit MVVM.Binding.LiveBindings.Controls.FMX;

interface

uses
  FMX.Grid,
  FMX.Grid.Style,
  Data.DB,

  MVVM.Controls.Platform.FMX,
  MVVM.Bindings.LiveBindings;

type
  // In order to optimize the refreshes
  TBindingHelperForGrid = class helper for TGrid
  public
    function RemoveDataSetBinding: Boolean;
    function DisableDataSetBinding: Boolean;
    function EnableDataSetBinding(ADataSet: TDataSet): Boolean;
  end;

implementation

uses
  System.RTTI,
  System.Classes,
  System.TypInfo,

  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  Spring.Collections,
  System.Generics.Collections,

  MVVM.Bindings.LiveBindings.EnumerableAdapter,

  Fmx.Bind.Grid,
  Fmx.Bind.DBEngExt,
  Fmx.Bind.Editors,
  Fmx.Bind.Navigator,

  Data.Bind.Controls,
  Data.Bind.Grid,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  Data.Bind.DBScope;

{ TBindingHelperForGrid }

function TBindingHelperForGrid.RemoveDataSetBinding: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Self.ComponentCount - 1 do
  begin
    if (Self.Components[I] is TBindSourceDB) then
    begin
      Self.Components[I].Free;
      Exit(True);
    end;
  end;
end;

function TBindingHelperForGrid.DisableDataSetBinding: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Self.ComponentCount - 1 do
  begin
    if (Self.Components[I] is TBindSourceDB) then
    begin
      TBindSourceDB(Self.Components[I]).DataSet := nil;
      Exit(True);
    end;
  end;
end;

function TBindingHelperForGrid.EnableDataSetBinding(ADataSet: TDataSet): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Self.ComponentCount - 1 do
  begin
    if (Self.Components[I] is TBindSourceDB) then
    begin
      TBindSourceDB(Self.Components[I]).DataSet := ADataSet;
      Exit(True);
    end;
  end;
end;

initialization

{$REGION 'TListBox'}
(*
  TStrategy_LiveBindings.RegisterClassObjectListCollectionBinder(TListBox,
  procedure(AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>)
  var
    LLink: TBindListLink;
    LSource: TAdapterBindSource;
    I: Integer;
  begin
    // Remove possible previous link
    for I := 0 to AComponent.ComponentCount - 1 do
    begin
      if (AComponent.Components[I] is TBindListLink) then
      begin
        AComponent.Components[I].Free;
        Break;
      end;
    end;
    LLink := TBindListLink.Create(AComponent);
    LLink.ControlComponent := AComponent;
    LSource := TAdapterBindSource.Create(LLink);
    LSource.Adapter := TListBindSourceAdapter.Create(AComponent, ACollection as TList<TObject>, GetTypeData(PTypeInfo(AServiceType))^.ClassType, True);
    LLink.SourceComponent := LSource;
  end);

TStrategy_LiveBindings.RegisterClassDataSetCollectionBinder(TListBox,
  procedure(ADataSet: TDataSet; AComponent: TComponent)
  var
    LLink: TBindListLink;
    LSource: TBindSourceDB;
    LDS: TDataSource;
    I: Integer;
  begin
    // Remove possible previous link
    for I := 0 to AComponent.ComponentCount - 1 do
    begin
      if (AComponent.Components[I] is TBindListLink) then
      begin
        AComponent.Components[I].Free;
        Break;
      end;
    end;
    LLink := TBindListLink.Create(AComponent);
    LLink.ControlComponent := AComponent;
    LDS := TDataSource.Create(LLink);
    LDS.DataSet := ADataSet;
    LSource := TBindSourceDB.Create(LLink);
    LSource.DataSource := LDS;
    LLink.SourceComponent := LSource;
  end);
*)
{$ENDREGION}

{$REGION 'TListView'}
(*
TStrategy_LiveBindings.RegisterClassObjectListCollectionBinder(TListView,
  procedure(AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>)
  var
    LLink: TBindListLink;
    LSource: TAdapterBindSource;
    I: Integer;
  begin
    // Remove possible previous link
    for I := 0 to AComponent.ComponentCount - 1 do
    begin
      if (AComponent.Components[I] is TBindListLink) then
      begin
        AComponent.Components[I].Free;
        Break;
      end;
    end;
    LLink := TBindListLink.Create(AComponent);
    LLink.ControlComponent := AComponent;
    LSource := TAdapterBindSource.Create(LLink);
    LSource.Adapter := TListBindSourceAdapter.Create(AComponent, ACollection as TList<TObject>, GetTypeData(PTypeInfo(AServiceType))^.ClassType, True);
    LLink.SourceComponent := LSource;
  end);

TStrategy_LiveBindings.RegisterClassDataSetCollectionBinder(TListView,
  procedure(ADataSet: TDataSet; AComponent: TComponent)
  var
    LLink: TBindListLink;
    LSource: TBindSourceDB;
    LDS: TDataSource;
    I: Integer;
  begin
    // Remove possible previous link
    for I := 0 to AComponent.ComponentCount - 1 do
    begin
      if (AComponent.Components[I] is TBindListLink) then
      begin
        AComponent.Components[I].Free;
        Break;
      end;
    end;
    LLink := TBindListLink.Create(AComponent);
    LLink.ControlComponent := AComponent;
    LDS := TDataSource.Create(LLink);
    LDS.DataSet := ADataSet;
    LSource := TBindSourceDB.Create(LLink);
    LSource.DataSource := LDS;
    LLink.SourceComponent := LSource;
  end);
{$ENDREGION}

{$REGION 'TGrid'}
TStrategy_LiveBindings.RegisterClassObjectListCollectionBinder(TGrid,
  procedure(AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>)
  var
    LLink: TBindListLink;
    LSource: TAdapterBindSource;
    I: Integer;
  begin
    // Remove possible previous link
    for I := 0 to AComponent.ComponentCount - 1 do
    begin
      if (AComponent.Components[I] is TBindListLink) then
      begin
        AComponent.Components[I].Free;
        Break;
      end;
    end;
    LLink := TBindListLink.Create(AComponent);
    LLink.ControlComponent := AComponent;
    LSource := TAdapterBindSource.Create(LLink);
    LSource.Adapter := TListBindSourceAdapter.Create(AComponent, ACollection as TList<TObject>, GetTypeData(PTypeInfo(AServiceType))^.ClassType, True);
    LLink.SourceComponent := LSource;
  end);
*)
{$ENDREGION}

end.
