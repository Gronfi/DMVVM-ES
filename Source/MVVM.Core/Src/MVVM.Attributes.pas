unit MVVM.Attributes;

interface

uses
  MVVM.Types,
  MVVM.Interfaces.Architectural;

type
  // Attributes
  EInstanceType = (itSingleton, itNewInstance);

  TMVVMCustomAttribute = class(TCustomAttribute)
  end;

  // [ViewForViewModel('EDITOR_USUARIOS', IMyViewModel1, 'WindowsDesktop']
  View_For_ViewModel = class(TMVVMCustomAttribute)
  strict private
    FViewAlias: string;
    FVMInterface: TGUID;
    FPlatform: string;
  public
    constructor Create(const AViewALias: string; AVMInterfaceID: TGUID; const APlatform: String);
    property ViewAlias: String read FViewAlias;
    property VMInterface: TGUID read FVMInterface;
    property Platform: String read FPlatform;
  end;

  // [ViewModel_Implements(ICSVFile_ViewModel, AsSingleton)]
  ViewModel_Implements = class(TMVVMCustomAttribute)
  strict private
    FVMInterface: TGUID;
    FInstanceType: EInstanceType;
  public
    constructor Create(AVMInterfaceID: TGUID; const AInstanceType: EInstanceType = EInstanceType.itNewInstance);
    property VMInterface: TGUID read FVMInterface;
    property InstanceType: EInstanceType read FInstanceType;
  end;

  // Commands
  // - Action Member
  ActionMember = class(TMVVMCustomAttribute)
  private
    FName: String;
    FCaption: String;
    FMemberType: EActionMemberType;
  public
    constructor Create(const AName: String; const AMemberType: EActionMemberType; const ACaption: String = ''); overload;
    constructor Create(const AName: String; const AMemberType: EActionMemberType); overload;
    constructor Create(const AMemberType: EActionMemberType; const ACaption: String = ''); overload;

    property Name: String read FName;
    property Caption: String read FCaption;
    property MemberType: EActionMemberType read FMemberType;
  end;

  // - Standard command
  Command = class(TMVVMCustomAttribute)
  private
    FExecuteName: String;
    FCanExecuteName: String;
    FParamsName: String;
  public
    constructor Create(const AExecuteName: string; const ACanExecuteName: String = ''); overload;
    constructor Create(const AExecuteName, ACanExecuteName: String; const AParamsName: string); overload;

    property ExecuteName: String read FExecuteName;
    property CanExecuteName: String read FCanExecuteName;
    property ParamsName: String read FParamsName;
  end;

implementation

{ ViewForVM }

constructor View_For_ViewModel.Create(const AViewALias: string; AVMInterfaceID: TGUID; const APlatform: String);
begin
  FVMInterface := AVMInterfaceID;
  FViewAlias   := AViewALias;
  FPlatform    := APlatform;
end;

{ ViewModel_Implements }

constructor ViewModel_Implements.Create(AVMInterfaceID: TGUID; const AInstanceType: EInstanceType);
begin
  FVMInterface  := AVMInterfaceID;
  FInstanceType := AInstanceType;
end;

{ ActionMember }

constructor ActionMember.Create(const AName: String; const AMemberType: EActionMemberType);
begin
  FName   := AName;
  FCaption:= '';
  FMemberType  := AMemberType;
end;

constructor ActionMember.Create(const AName: String; const AMemberType: EActionMemberType; const ACaption: String = '');
begin
  FName   := AName;
  FCaption:= ACaption;
  FMemberType  := AMemberType;
end;

constructor ActionMember.Create(const AMemberType: EActionMemberType; const ACaption: String);
begin
  FName := '';
  FCaption:= ACaption;
  FMemberType  := AMemberType;
end;

{ Command }

constructor Command.Create(const AExecuteName, ACanExecuteName, AParamsName: string);
begin
  FExecuteName := AExecuteName;
  FCanExecuteName:= ACanExecuteName;
  FParamsName:= AParamsName;
end;

constructor Command.Create(const AExecuteName, ACanExecuteName: String);
begin
  FExecuteName := AExecuteName;
  FCanExecuteName:= ACanExecuteName;
  FParamsName:= '';
end;

end.
