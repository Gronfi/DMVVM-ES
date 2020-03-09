unit MVVM.Classes;

interface

uses
  MVVM.Interfaces,
  MVVM.Types;

type
  TViewModel<T:IModel> = class abstract(TInterfacedObject, IViewModel<T>)
  private
    FModel: T;
  protected
    function GetModel: T; virtual;
  public
    procedure SetModel(AModel: T); virtual;
    procedure SetupViewModel; virtual; abstract;

    property Model: T read GetModel;
  end;

implementation

uses
  Spring;

{ TViewModel<T> }

function TViewModel<T>.GetModel: T;
begin
  Guard.CheckNotNull(FModel, 'The model is not assigned');
  Result := FModel;
end;

procedure TViewModel<T>.SetModel(AModel: T);
begin
  FModel := AModel;
end;

end.
