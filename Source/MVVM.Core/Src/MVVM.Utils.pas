unit MVVM.Utils;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  //System.Rtti,
  System.SysUtils,
  System.Classes,
  System.UITypes,

  Spring,

  MVVM.Interfaces.Architectural;

type
  TCustomAttributeClass = class of TCustomAttribute;

  Utils = record
  public
    class function CreateEvent<T>: IEvent<T>; static;
{$IFDEF MSWINDOWS}
    class procedure IdeDebugMsg(const AMsg: String); static;
{$ENDIF}
    class function iif<T>(const ACondition: Boolean; AResult_True, AResult_False: T): T; overload; static;
    class function iif<T>(const ACondition: Boolean; AResult_True, AResult_False: TFunc<T>): T; overload; static;
    class function StringToCaseSelect(const Selector: string; const CaseList: array of string): integer; static;
    class function InterfaceToCaseSelect(Selector: IInterface; const CaseList: array of TGUID): integer; static;
    class function AttributeToCaseSelect(Selector: TCustomAttribute; const CaseList: array of TCustomAttributeClass): integer; static;
    class function ShowView<I: IViewModel>(AViewModel: I; const AViewName: string; const APlatform: String = ''; const AOwner: TComponent = nil): IView<I>; overload; static;
    class procedure ShowView(AView: IView); overload; static;
    class function ShowModalView<I: IViewModel>(AViewModel: I; const AViewName: string; const AResultProc: TProc<TModalResult>; const APlatform: String = ''; const AOwner: TComponent = nil): IView<I>; overload; static;
    class procedure ShowModalView(AView: IView; const AResultProc: TProc<TModalResult>); overload; static;
    class function StyledFieldOfComponent(const AField: String): String; static;
  end;

implementation

uses
  MVVM.Core,
  System.TypInfo;

{ Utils }

class function Utils.AttributeToCaseSelect(Selector: TCustomAttribute; const CaseList: array of TCustomAttributeClass): integer;
var
  LCnt: integer;
begin
  Result   := -1;
  for LCnt := 0 to Length(CaseList) - 1 do
  begin
    if (Selector is CaseList[LCnt]) then
    begin
      Result := LCnt;
      break;
    end;
  end;
end;

class function Utils.CreateEvent<T>: IEvent<T>;
var
  E: Event<T>;
begin
  Result := E;
end;

class procedure Utils.IdeDebugMsg(const AMsg: String);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(FormatDateTime('hhnnss.zzz', Now) + AMsg));
{$ENDIF}
end;

class function Utils.iif<T>(const ACondition: Boolean; AResult_True, AResult_False: TFunc<T>): T;
begin
  if ACondition then
    Result := AResult_True
  else
    Result := AResult_False;
end;

class function Utils.iif<T>(const ACondition: Boolean; AResult_True, AResult_False: T): T;
begin
  if ACondition then
    Result := AResult_True
  else
    Result := AResult_False;
end;

class function Utils.InterfaceToCaseSelect(Selector: IInterface; const CaseList: array of TGUID): integer;
var
  LCnt: integer;
begin
  Result   := -1;
  for LCnt := 0 to Length(CaseList) - 1 do
  begin
    if Supports(Selector, CaseList[LCnt]) then
    begin
      Result := LCnt;
      break;
    end;
  end;
end;

class function Utils.ShowModalView<I>(AViewModel: I; const AViewName: string; const AResultProc: TProc<TModalResult>; const APlatform: String; const AOwner: TComponent): IView<I>;
var
  [weak]
  LViewForm: IViewForm<I>;
begin
  Result := MVVMCore.ViewsProvider.CreateView<I>(APlatform, AViewName, AOwner, AViewModel);
  if Supports(Result, IView<I>, LViewForm) then
  begin
    LViewForm.ExecuteModal(AResultProc);
  end;
end;

class procedure Utils.ShowModalView(AView: IView; const AResultProc: TProc<TModalResult>);
begin
  MVVMCore.PlatformServices.ShowModalFormView(AView.GetAsObject as TComponent, AResultProc);
end;

class function Utils.ShowView<I>(AViewModel: I; const AViewName, APlatform: String; const AOwner: TComponent): IView<I>;
var
  [weak]
  LViewForm: IViewForm<I>;
begin
  Result := MVVMCore.ViewsProvider.CreateView<I>(APlatform, AViewName, AOwner, AViewModel);
  if Supports(Result, IView<I>, LViewForm) then
  begin
    LViewForm.Execute;
  end;
end;

class procedure Utils.ShowView(AView: IView);
begin
  MVVMCore.PlatformServices.ShowFormView(AView.GetAsObject as TComponent);
end;

class function Utils.StringToCaseSelect(const Selector: string; const CaseList: array of string): integer;
var
  LCnt: integer;
begin
  Result   := -1;
  for LCnt := 0 to Length(CaseList) - 1 do
  begin
    if CompareText(Selector, CaseList[LCnt]) = 0 then
    begin
      Result := LCnt;
      break;
    end;
  end;
end;

class function Utils.StyledFieldOfComponent(const AField: String): String;
begin
  Result := 'StylesData[''' + AField + ''']';
end;

end.
