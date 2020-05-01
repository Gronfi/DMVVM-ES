unit Coche.New.VM;

interface

uses
  System.SysUtils,
  System.Classes,
  Data.DB,

  Spring,
  Spring.Collections,

  MVVM.Types,
  MVVM.Attributes,
  MVVM.Interfaces,
  MVVM.Interfaces.Architectural,
  MVVM.Bindings,

  Coche.Interfaces,
  Coche.Types;

type
  [ViewModel_Implements(INewCoche_ViewModel)]
  TNewCoche_ViewModel = class(TViewModel, INewCoche_ViewModel)
  protected
    FData          : RCoche;
    FOnDataSelected: IEvent<TNotify_Coche>;

    function GetSetData: TExecuteMethod<RCoche>;
    function GetOnDataSelected: IEvent<TNotify_Coche>;

    procedure DataChanged;
  public
    procedure SetData(const AData: RCoche);

    property DoSetData: TExecuteMethod<RCoche> read GetSetData;
    property OnDataSelected: IEvent<TNotify_Coche> read GetOnDataSelected;
  end;

implementation

uses
  MVVM.Utils;

{ TNewCoche_ViewModel }

procedure TNewCoche_ViewModel.DataChanged;
begin
  if Assigned(FOnDataSelected) then
    FOnDataSelected.Invoke(FData)
end;

function TNewCoche_ViewModel.GetOnDataSelected: IEvent<TNotify_Coche>;
begin
  if not Assigned(FOnDataSelected) then
    FOnDataSelected := Utils.CreateEvent<TNotify_Coche>;
  Result := FOnDataSelected;
end;

function TNewCoche_ViewModel.GetSetData: TExecuteMethod<RCoche>;
begin
  Result := SetData;
end;

procedure TNewCoche_ViewModel.SetData(const AData: RCoche);
begin
  FData := AData;
  DataChanged;
end;

end.
