unit MVVM.Bindings.Commands;

interface

uses
  System.Actions,

  MVVM.Interfaces;

type

  TBindingCommandAction = class(TBindingCommandBase<TContainedAction>)
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
