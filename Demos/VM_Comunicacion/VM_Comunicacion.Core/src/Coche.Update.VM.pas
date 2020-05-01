unit Coche.Update.VM;

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
  [ViewModel_Implements(IUpdateCoche_ViewModel)]
  TNewCoche_ViewModel = class(TViewModel, IUpdateCoche_ViewModel)
  protected
    FData          : RCoche;
    FOnDataSelected: IEvent<TNotify_Coche>;

    function GetSetData: TExecuteMethod<RCoche>;

    function GetData: RCoche;
    procedure SetData(const AData: RCoche);

    procedure DataChanged;

    function GetOnDataSelected: IEvent<TNotify_Coche>;
  public
    property DoSetData: TExecuteMethod<RCoche> read GetSetData;
    property OnDataSelected: IEvent<TNotify_Coche> read GetOnDataSelected;
    property Data: RCoche read GetData write SetData;
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

function TNewCoche_ViewModel.GetData: RCoche;
begin
  Result := FData;
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

