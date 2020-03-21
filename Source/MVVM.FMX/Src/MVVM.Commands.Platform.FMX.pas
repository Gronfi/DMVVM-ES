unit MVVM.Commands.Platform.FMX;

interface

uses
  MVVM.Interfaces,
  MVVM.Controls.Platform.FMX;

type

  TBindingCommandAction = class(TBindingCommandBase<TAction>)
  protected
    procedure DoEnabled; override;
    procedure DoDisabled; override;
  public
    procedure Execute; override;
  end;

implementation

{ TBindingCommandAction }

procedure TBindingCommandAction.DoDisabled;
begin
  FCommand.Enabled := False;
end;

procedure TBindingCommandAction.DoEnabled;
begin
  FCommand.Enabled := True;
end;

procedure TBindingCommandAction.Execute;
begin
  if Enabled then
    if CanExecute then
      FCommand.Execute;
end;

end.
