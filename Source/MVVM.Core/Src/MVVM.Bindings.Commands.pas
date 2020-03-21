unit MVVM.Bindings.Commands;

interface

uses
  System.RTTI,
  System.SysUtils,
  System.Classes,

  MVVM.Interfaces;

type
  TBindingCommandRttiMethod = class(TBindingCommandBase<TRttiMethod>)
  public
    procedure Execute; override;
  end;

  TBindingCommandAnonimousMethod = class(TBindingCommandBase<TProc>)
  public
    procedure Execute; override;
  end;

implementation

{ TBindingCommandMethod }

procedure TBindingCommandRttiMethod.Execute;
begin
  if Enabled then
    Self.Command.Invoke(Self.Owner, []);
end;

{ TBindingCommandAnonimousMethod }

procedure TBindingCommandAnonimousMethod.Execute;
begin
  if Enabled then
    Self.Command();
end;

end.
