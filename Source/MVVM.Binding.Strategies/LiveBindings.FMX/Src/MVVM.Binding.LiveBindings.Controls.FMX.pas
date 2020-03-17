unit MVVM.Binding.LiveBindings.Controls.FMX;

{$HPPEMIT 'MVVM.Binding.LiveBindings.Controls.FMX'}

interface

uses
  MVVM.Controls.Platform.FMX,
  MVVM.Bindings.LiveBindings;

implementation

uses
  System.RTTI,
  System.Classes,
  System.TypInfo,
  Data.DB,

  Spring.Collections,
  System.Generics.Collections,

  Data.Bind.Components,
  Data.Bind.ObjectScope,
  Data.Bind.DBScope;

initialization

{$REGION 'TListBox'}
TStrategy_LiveBindings.RegisterClassObjectListCollectionBinder(TListBox,
  procedure (AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>)
  var
    LLink  : TBindListLink;
    LSource: TAdapterBindSource;
    I      : Integer;
  begin
    //Remove possible previous link
    for I := 0 to AComponent.ComponentCount - 1 do
    begin
      if (AComponent.Components[I] is TBindListLink) then
      begin
        AComponent.Components[I].Free;
        Break;
      end;
    end;
    LLink                  := TBindListLink.Create(AComponent);
    LLink.ControlComponent := AComponent;
    LSource                := TAdapterBindSource.Create(LLink);
    LSource.Adapter        := TListBindSourceAdapter.Create(AComponent, ACollection as TList<TObject>, GetTypeData(PTypeInfo(AServiceType))^.ClassType, True);
    LLink.SourceComponent  := LSource;
  end);

TStrategy_LiveBindings.RegisterClassDataSetCollectionBinder(TListBox,
  procedure (ADataSet: TDataSet; AComponent: TComponent)
  var
    LLink  : TBindListLink;
    LSource: TBindSourceDB;
    LDS    : TDataSource;
    I      : Integer;
  begin
    //Remove possible previous link
    for I := 0 to AComponent.ComponentCount - 1 do
    begin
      if (AComponent.Components[I] is TBindListLink) then
      begin
        AComponent.Components[I].Free;
        Break;
      end;
    end;
    LLink                  := TBindListLink.Create(AComponent);
    LLink.ControlComponent := AComponent;
    LDS                    := TDataSource.Create(LLink);
    LDS.DataSet            := ADataSet;
    LSource                := TBindSourceDB.Create(LLink);
    LSource.DataSource     := LDS;
    LLink.SourceComponent  := LSource;
  end);
{$ENDREGION}
end.
