unit MVVM.Utils;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils,

  Spring;

type
  Utils = record
  public
    class function CreateEvent<T>: IEvent<T>; static;
    class procedure IdeDebugMsg(const AMsg: String); static;
    class function iif<T>(const ACondition: Boolean;
      AResult_True, AResult_False: T): T; overload; static;
    class function iif<T>(const ACondition: Boolean;
      AResult_True, AResult_False: TFunc<T>): T; overload; static;
  end;

implementation

{ Utils }

class function Utils.CreateEvent<T>: IEvent<T>;
var
  E: Event<T>;
begin
  Result := E;
end;

class procedure Utils.IdeDebugMsg(const AMsg: String);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AMsg));
{$ENDIF}
end;

class function Utils.iif<T>(const ACondition: Boolean;
  AResult_True, AResult_False: TFunc<T>): T;
begin
  if ACondition then
    Result := AResult_True
  else
    Result := AResult_False;
end;

class function Utils.iif<T>(const ACondition: Boolean;
  AResult_True, AResult_False: T): T;
begin
  if ACondition then
    Result := AResult_True
  else
    Result := AResult_False;
end;

end.
