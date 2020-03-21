unit MVVM.Commands.Platform.FMX;

interface

uses
  MVVM.Interfaces,
  MVVM.Controls.Platform.FMX;

type

  TBindingCommandAction = class(TBindingCommandBase<TAction>)
  public
    procedure Execute; override;
  end;

implementation

{ TBindingCommandAction }

procedure TBindingCommandAction.Execute;
begin
  if Enabled then
    FCommand.Execute;
end;

end.
