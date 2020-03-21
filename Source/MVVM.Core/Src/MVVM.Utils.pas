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
    class function StringToCaseSelect(const Selector: string; const CaseList: array of string): integer; static;
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

class function Utils.StringToCaseSelect(const Selector: string; const CaseList: array of string): integer;
var
  cnt: integer;
begin
  Result  := -1;
  for cnt := 0 to Length(CaseList) - 1 do
  begin
    if CompareText(Selector, CaseList[cnt]) = 0 then
    begin
      Result := cnt;
      Break;
    end;
  end;
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
