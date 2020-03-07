unit MVVM.Controls.Platform.FMX;

interface

uses
  System.Rtti,
  System.Classes,
  FMX.Controls.Model,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Memo,
  FMX.ComboEdit,
  FMX.Colors,
  FMX.DateTimeCtrls,
  FMX.SpinBox,
  FMX.NumberBox,
  FMX.ListBox,
  FMX.ListView,
  FMX.Objects,
  FMX.ActnList,

  MVVM.Interfaces,
  MVVM.Types;

type

{$REGION 'FMX.ActnList'}
  TAction = class(FMX.ActnList.TAction, IBindableAction)
  {$REGION 'Internal Declarations'}
  private
    FExecute   : TExecuteMethod;
    //FExecuteInt: TExecuteMethod<Integer>;
    FCanExecute: TCanExecuteMethod;
  {$ENDREGION 'Internal Declarations'}
  public
    { IBindableAction }
    procedure Bind(const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod = nil); overload;
    //procedure Bind(const AExecute: TgoExecuteMethod<Integer>;
    //  const ACanExecute: TgoCanExecuteMethod = nil); overload;
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;
{$ENDREGION 'FMX.ActnList'}

implementation

{ TAction }

procedure TAction.Bind(const AExecute: TExecuteMethod; const ACanExecute: TCanExecuteMethod);
begin
  FExecute    := AExecute;
  FCanExecute := ACanExecute;
end;

constructor TAction.Create(AOwner: TComponent);
begin
  inherited;
  DisableIfNoHandler := False;
end;

function TAction.Execute: Boolean;
begin
  Result := inherited Execute;
  if (Supported) and (not Suspended) and (Enabled) then
  begin
    if Assigned(FExecute) then
      FExecute();
    //else if Assigned(FExecuteInt) then
    //  FExecuteInt(Tag);
  end;
end;

function TAction.Update: Boolean;
begin
  Result := inherited Update;
  if (Supported) and Assigned(FCanExecute) then
    Enabled := FCanExecute();
end;

end.
