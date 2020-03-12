unit MVVM.Binding.LiveBindings.Controls.FMX;

interface

uses
  MVVM.Controls.Platform.FMX,
  MVVM.Bindings.LiveBindings;

implementation

uses
  System.RTTI,
  System.Classes,
  System.TypInfo,

  Spring.Collections,
  System.Generics.Collections,

  Data.Bind.Components,
  Data.Bind.ObjectScope,
  Data.Bind.DBScope;

initialization

TEstrategia_LiveBindings.RegisterClassObjectListCollectionBinder(TListBox,
  procedure (AServiceType: PTypeInfo; AComponent: TComponent; ACollection: TEnumerable<TObject>)
  var
    LLink  : TBindListLink;
    LSource: TAdapterBindSource;
  begin
    LSource                := TAdapterBindSource.Create(AComponent);
    LSource.Adapter        := TListBindSourceAdapter.Create(AComponent, ACollection as TList<TObject>, GetTypeData(PTypeInfo(AServiceType))^.ClassType, True);
    LLink                  := TBindListLink.Create(AComponent);
    LLink.ControlComponent := AComponent;
    LLink.SourceComponent  := LSource;
  end);

end.
