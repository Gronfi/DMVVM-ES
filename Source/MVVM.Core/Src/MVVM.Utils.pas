unit MVVM.Utils;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.Rtti,
  System.SysUtils,
  System.Classes,
  System.UITypes,

  Spring,

  MVVM.Interfaces.Architectural;

type
  TCustomAttributeClass =  class of TCustomAttribute;

  Utils = record
  public
    class function CreateEvent<T>: IEvent<T>; static;
{$IFDEF MSWINDOWS}
    class procedure IdeDebugMsg(const AMsg: String); static;
{$ENDIF}
    class function iif<T>(const ACondition: Boolean; AResult_True, AResult_False: T): T; overload; static;
    class function iif<T>(const ACondition: Boolean; AResult_True, AResult_False: TFunc<T>): T; overload; static;
    class function StringToCaseSelect(const Selector: string; const CaseList: array of string): integer; static;
    class function InterfaceToCaseSelect(Selector: IInterface; const CaseList: array of TGUID): Integer; static;
    class function AttributeToCaseSelect(Selector: TCustomAttribute; const CaseList: array of TCustomAttributeClass): Integer; static;
    class function GetInterfaceTypeInfo(const GUID: TGUID): PTypeInfo; static;
    class function CreateComponent_From_RttiInstance(ARttiInstance: TRttiInstanceType; const ACreateParams: array of TValue): TComponent; static;
    class function ShowView<I:IViewModel>(AViewModel: I; const AViewName: string; const APlatform: String = ''; const AOwner: TComponent = nil): IView<I>; overload; static;
    class procedure ShowView(AView: IView); overload; static;
    class function ShowModalView<I:IViewModel>(AViewModel: I; const AViewName: string; const AResultProc: TProc<TModalResult>; const APlatform: String = ''; const AOwner: TComponent = nil): IView<I>; overload; static;
    class procedure ShowModalView(AView: IView; const AResultProc: TProc<TModalResult>); overload; static;
    //MVVMCore.DefaultViewPlatform, ICSVFile_View_NAME, nil, VistaModelo);
  end;

implementation

uses
  MVVM.Core,
  System.TypInfo;

{ Utils }

class function Utils.AttributeToCaseSelect(Selector: TCustomAttribute; const CaseList: array of TCustomAttributeClass): Integer;
var
  LCnt: Integer;
begin
  Result  := -1;
  for LCnt := 0 to Length(CaseList) - 1 do
  begin
    if (Selector is CaseList[LCnt]) then
    begin
      Result := LCnt;
      break;
    end;
  end;
end;

class function Utils.CreateComponent_From_RttiInstance(ARttiInstance: TRttiInstanceType; const ACreateParams: array of TValue): TComponent;
var
  LCreateMethod: TRttiMethod;
begin
  // Loop for all methods
  LCreateMethod := nil;
  for LCreateMethod in ARttiInstance.GetMethods do
  begin
    if  (LCreateMethod.HasExtendedInfo) and (LCreateMethod.IsConstructor) and (Length(LCreateMethod.GetParameters) = Length(ACreateParams)) then
    begin
      Break;
    end;
  end;
  if not Assigned(LCreateMethod) then
    raise Exception.Create('Constructor not found for class: ' + ARttiInstance.QualifiedClassName);
  Result := LCreateMethod.Invoke(ARttiInstance.MetaclassType, ACreateParams).AsObject as TComponent;
end;

class function Utils.CreateEvent<T>: IEvent<T>;
var
  E: Event<T>;
begin
  Result := E;
end;

class function Utils.GetInterfaceTypeInfo(const GUID: TGUID): PTypeInfo;
var
  Ctx: TRttiContext;
  AType: TRttiType;
begin
  Result := nil;
  Ctx := TRttiContext.Create;
  try
    for AType in Ctx.GetTypes do
      if (AType.TypeKind = tkInterface) and IsEqualGUID(GetTypeData(AType.Handle)^.Guid, GUID) then
      begin
        Result := PTypeInfo(AType); //.Handle;
        Break;
      end;
  finally
    Ctx.Free;
  end;
end;

class procedure Utils.IdeDebugMsg(const AMsg: String);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AMsg));
{$ENDIF}
end;

class function Utils.iif<T>(const ACondition: Boolean; AResult_True, AResult_False: TFunc<T>): T;
begin
  if ACondition then
    Result := AResult_True
  else
    Result := AResult_False;
end;

class function Utils.InterfaceToCaseSelect(Selector: IInterface; const CaseList: array of TGUID): Integer;
var
  LCnt: Integer;
begin
  Result  := -1;
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
  [weak] LViewForm: IViewForm<I>;
begin
  Result := MVVMCore.ViewsProvider.CreateView<I>(APlatform, AViewName, AOwner, AViewModel);
  if Supports(Result, IView<I>, LViewForm)  then
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
  [weak] LViewForm: IViewForm<I>;
begin
  Result := MVVMCore.ViewsProvider.CreateView<I>(APlatform, AViewName, AOwner, AViewModel);
  if Supports(Result, IView<I>, LViewForm)  then
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
  Result  := -1;
  for LCnt := 0 to Length(CaseList) - 1 do
  begin
    if CompareText(Selector, CaseList[LCnt]) = 0 then
    begin
      Result := LCnt;
      Break;
    end;
  end;
end;

class function Utils.iif<T>(const ACondition: Boolean; AResult_True, AResult_False: T): T;
begin
  if ACondition then
    Result := AResult_True
  else
    Result := AResult_False;
end;

end.
