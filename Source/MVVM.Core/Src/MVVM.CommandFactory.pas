unit MVVM.CommandFactory;

interface

uses
  System.Rtti,

  Spring.Collections,

  MVVM.Interfaces,
  MVVM.Types;

type
  TCommandFactory = class
{$REGION 'Internal Declarations'}
  private
    class var FRegisteredActionMembers: IDictionary<String, RActionMember>;
  public
    //class destructor Destroy;
{$ENDREGION 'Internal Declarations'}
  public
    class procedure CheckMethodIsRightMemberType(const AActionMemberType: EActionMemberType; const AMethod: TRttiMethod); static; //basic test
    class procedure RegisterActionMember(const AName: string; const ACaption: String; const AActionMemberType: EActionMemberType; const AMethod: TRttiMethod); static;
    class function GetActionMember(const AName: string): RActionMember; static;
//
//    { Creates a view using a previously registered view class.
//      Returns:
//      A newly created view that represents AViewName.
//      Raises:
//      EListError if there is no view registered with the given AViewName. }
//    class function CreateView<TVM: IViewModel>(const APlatform: string; const AViewName: String; const AOwner: TComponent; AViewModel: TVM): IView<TVM>; static;
  end;

implementation

uses
  System.TypInfo,
  System.SysUtils;

{ TCommandFactory }

class procedure TCommandFactory.CheckMethodIsRightMemberType(const AActionMemberType: EActionMemberType; const AMethod: TRttiMethod);
begin
  case AActionMemberType of
    OnExecute:
      begin
        if AMethod.MethodKind <> TMethodKind.mkProcedure then
          raise ExceptionActionMemberTypeError.Create('The member type or the method are wrong');
      end;
    OnUpdate:
      begin
        if AMethod.MethodKind <> TMethodKind.mkFunction then
          raise ExceptionActionMemberTypeError.Create('The member type or the method are wrong');
      end;
    OnAsyncExecutionFinished:
      begin
        if AMethod.MethodKind <> TMethodKind.mkProcedure then
          raise ExceptionActionMemberTypeError.Create('The member type or the method are wrong');
      end;
    OnParams:
      begin
        if AMethod.MethodKind <> TMethodKind.mkFunction then
          raise ExceptionActionMemberTypeError.Create('The member type or the method are wrong');
      end;
  end;
end;

class function TCommandFactory.GetActionMember(const AName: string): RActionMember;
var
  LName: string;
begin
  if AName.IsEmpty then
    raise ExceptionActionMemberNameCannotBeEmpty.Create('Member name cannot be empty');
  LName := AName.ToUpper;
  if not FRegisteredActionMembers.ContainsKey(LName) then
    raise ExceptionActionMemberNotFound.Create('Action member not found: ' + AName);
  Result := FRegisteredActionMembers[LName];
end;

class procedure TCommandFactory.RegisterActionMember(const AName, ACaption: String; const AActionMemberType: EActionMemberType; const AMethod: TRttiMethod);
var
  LName: string;
  LData: RActionMember;
begin
  LName := AName.ToUpper;
  if FRegisteredActionMembers = nil then
    FRegisteredActionMembers := TCollections.CreateDictionary<String, RActionMember>;
  if FRegisteredActionMembers.ContainsKey(LName) then
    raise ExceptionActionMemberTypeDuplicated.Create('The Name is already used: ' + AName);

  LData.Name := LName;
  LData.Caption := ACaption;
  LData.MemberType := AActionMemberType;
  LData.Method := AMethod;

  //Integrity Checking
  CheckMethodIsRightMemberType(AActionMemberType, AMethod);

  FRegisteredActionMembers[LName] := LData;
end;

end.
