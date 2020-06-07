unit MVVM.CommandFactory;

interface

uses
  System.Rtti,
  System.Classes,

  Spring.Collections,

  MVVM.Rtti,
  MVVM.Interfaces,
  MVVM.Types;

type
  TCommandsFactory = class
{$REGION 'Internal Declarations'}
  private
    FRegisteredActionMembers: IDictionary<String, RActionMember>;
    FRegisteredCommands: IList<RCommand>;
  protected
    //Actions
    procedure CheckMethodIsRightActionMemberType(const AActionMemberType: EActionMemberType; const AMethod: TRttiMethod); //basic test
    procedure RegisterActionMember(const AName: string; const ACaption: String; const AActionMemberType: EActionMemberType; const AMethod: TRttiMethod; const AOwner: TObject);
    function TryGetActionMember(const AName: string; out AData: RActionMember): Boolean;
    //Commands
    procedure RegisterCommand(const AExecuteName: string; const ACanExecuteName: String; const AParamsName: string; const AOwner: TObject; const AField: TRttiField);
  public
    //class destructor Destroy;
{$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadCommandsAndActionsFrom(const AObject: TObject);
    procedure BindView(const AView: TComponent);
  end;

implementation

uses
  System.TypInfo,
  System.SysUtils,

  MVVM.Utils,
  MVVM.Attributes;

{ TCommandFactory }

procedure TCommandsFactory.BindView(const AView: TComponent);
var
  LCommand   : RCommand;
  LExecute   : RActionMember;
  LCanExecute: RActionMember;
  LParams    : RActionMember;
  LUseCanExecute : Boolean;
  LUseParams     : Boolean;
  LBindableAction: IBindableAction;
  LObject        : TObject;
begin
  for LCommand in FRegisteredCommands do
  begin
    if (LCommand.Owner = AView) then //if the View is the good one
    begin
      LUseCanExecute:= False;
      LUseParams    := False;
      // Execute
      if not TryGetActionMember(LCommand.ExecuteName, LExecute) then
        raise ExceptionActionMemberNotFound.Create('Action Member not found: ' + LCommand.ExecuteName);
      // CanExecute
      if not LCommand.CanExecuteName.IsEmpty then
      begin
        if not TryGetActionMember(LCommand.CanExecuteName, LCanExecute) then
          raise ExceptionActionMemberNotFound.Create('Action Member not found: ' + LCommand.CanExecuteName);
        LUseCanExecute := True;
      end;
      // Params
      if not LCommand.ParamsName.IsEmpty then
      begin
        if not TryGetActionMember(LCommand.ParamsName, LParams) then
          raise ExceptionActionMemberNotFound.Create('Action Member not found: ' + LCommand.ParamsName);
        LUseParams    := True;
      end;
      LObject := LCommand.Field.GetValue(LCommand.Owner).AsType<TObject>;
      if Supports(LObject, IBindableAction, LBindableAction) then
      begin
        LBindableAction.Bind(LExecute.Method, LExecute.Owner,
                             Utils.iif<TRttiMethod>(LUseCanExecute, LCanExecute.Method, nil), Utils.iif<TObject>(LUseCanExecute, LCanExecute.Owner, nil),
                             Utils.iif<TRttiMethod>(LUseParams, LParams.Method, nil), Utils.iif<TObject>(LUseParams, LParams.Owner, nil));
      end;
    end;
  end;
end;

procedure TCommandsFactory.CheckMethodIsRightActionMemberType(const AActionMemberType: EActionMemberType; const AMethod: TRttiMethod);
begin
  (*
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
  *)
end;

constructor TCommandsFactory.Create;
begin
  inherited;
  FRegisteredActionMembers := TCollections.CreateDictionary<String, RActionMember>;
  FRegisteredCommands      := TCollections.CreateList<RCommand>;
end;

destructor TCommandsFactory.Destroy;
begin
  FRegisteredActionMembers := nil;
  FRegisteredCommands      := nil;
  inherited;
end;

function TCommandsFactory.TryGetActionMember(const AName: string; out AData: RActionMember): Boolean;
var
  LName: string;
begin
  if AName.IsEmpty then
    raise ExceptionActionMemberNameCannotBeEmpty.Create('Member name cannot be empty');
  LName := AName.ToUpper;
  Result := FRegisteredActionMembers.TryGetValue(LName, AData);
end;

procedure TCommandsFactory.LoadCommandsAndActionsFrom(const AObject: TObject);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  LMethod: TRttiMethod;
  LField: TRttiField;
  LProperty: TRttiProperty;
  LAttr: TCustomAttribute;
  LActionMember: ActionMember;
  LCommand: Command;
//  LTValueMethod: TValue;
begin
  Ctx := TRttiContext.Create;
  try
    //for Typ in Ctx.GetTypes do
    //begin
      // Loop for Method attributes
(*
      for LMethod in Typ.GetMethods do
      begin
        for LAttr in LMethod.GetAttributes do
        begin
          case Utils.AttributeToCaseSelect(LAttr, [ActionMember]) of
            0: // ActionMember
              begin
                LActionMember := LAttr as ActionMember;
                LConverted := Ctx.GetType(TypeInfo(LMethod.CodeAddress)) as TRttiInvokableType;

                if LTmp is TRttiInvokableType then
                  ;
                //LConverted := LMethod as TRttiInvokableType;
//                LTValueMethod := LMethod. as
                  //RegisterActionMember(LActionMember.Name, LActionMember.Caption, LActionMember.MemberType, LMethod, AObject);
              end;
          end;
        end;
      end;
*)
      Typ := ctx.GetType(AObject.ClassInfo); //ClassType
      // Loop for Properties attributes
      for LProperty in Typ.GetProperties do
      begin
        for LAttr in LProperty.GetAttributes do
        begin
          case Utils.AttributeToCaseSelect(LAttr, [ActionMember]) of
            0: // ActionMember
              begin
                LActionMember := LAttr as ActionMember;
                LMethod := LProperty.GetterMethod(AObject);
                if Assigned(LMethod) then
                  RegisterActionMember(LActionMember.Name, LActionMember.Caption, LActionMember.MemberType, LMethod, AObject);
              end;
          end;
        end;
      end;
      // Loop for Method attributes
      for LMethod in Typ.GetMethods do
      begin
        for LAttr in LMethod.GetAttributes do
        begin
          case Utils.AttributeToCaseSelect(LAttr, [ActionMember]) of
            0: // ActionMember
              begin
                LActionMember := LAttr as ActionMember;
                RegisterActionMember(LActionMember.Name, LActionMember.Caption, LActionMember.MemberType, LMethod, AObject);
              end;
          end;
        end;
      end;
      // Loop for Field attributes
      for LField in Typ.GetFields do
      begin
        for LAttr in LField.GetAttributes do
        begin
          case Utils.AttributeToCaseSelect(LAttr, [Command, ActionMember]) of
            0: // Command
              begin
                LCommand := LAttr as Command;
                RegisterCommand(LCommand.ExecuteName, LCommand.CanExecuteName, LCommand.ParamsName, AObject, LField);
              end;
          end;
        end;
      end;
  finally
    Ctx.Free;
  end;
end;

procedure TCommandsFactory.RegisterActionMember(const AName, ACaption: String; const AActionMemberType: EActionMemberType; const AMethod: TRttiMethod; const AOwner: TObject);
var
  LName: string;
  LData: RActionMember;
begin
  LName := AName.ToUpper;
  //Utils.IdeDebugMsg('<TCommandsFactory.RegisterActionMember> ' + AName);
  if LName.IsEmpty then
  begin
    LName := AMethod.Name.ToUpper; // For future uses
  end;
  if FRegisteredActionMembers = nil then
    FRegisteredActionMembers := TCollections.CreateDictionary<String, RActionMember>;
  if FRegisteredActionMembers.ContainsKey(LName) then
    raise ExceptionActionMemberTypeDuplicated.Create('The Name is already used: ' + AName);

  LData.Name := LName;
  LData.Caption := ACaption;
  LData.MemberType := AActionMemberType;
  LData.Method := AMethod;
  LData.Owner  := AOwner;

  //Integrity Checking
  CheckMethodIsRightActionMemberType(AActionMemberType, AMethod);

  FRegisteredActionMembers[LName] := LData;
end;

procedure TCommandsFactory.RegisterCommand(const AExecuteName, ACanExecuteName, AParamsName: string; const AOwner: TObject; const AField: TRttiField);
var
  LCommand: RCommand;
begin
  LCommand.ExecuteName    := AExecuteName;
  LCommand.CanExecuteName := ACanExecuteName;
  LCommand.ParamsName     := AParamsName;
  LCommand.Owner          := AOwner;
  LCommand.Field          := AField;
  if FRegisteredCommands = nil then
    FRegisteredCommands := TCollections.CreateList<RCommand>;
  FRegisteredCommands.Add(LCommand);
end;

end.
