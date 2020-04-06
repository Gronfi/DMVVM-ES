unit MVVM.Attributes;

interface

uses
  MVVM.Types,
  MVVM.Interfaces.Architectural;

type
  //Attributes
  EInstanceType = (itSingleton, itNewInstance);

  TMVVMCustomAttribute = class(TCustomAttribute)
  end;

  //[ViewForViewModel('EDITOR_USUARIOS', IMyViewModel1, 'WindowsDesktop']
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

  //[ViewModel_Implements(ICSVFile_ViewModel, AsSingleton)]
  ViewModel_Implements = class(TMVVMCustomAttribute)
  strict private
    FVMInterface : TGUID;
    FInstanceType: EInstanceType;
  public
    constructor Create(AVMInterfaceID: TGUID; const AInstanceType: EInstanceType = EInstanceType.itNewInstance);
    property VMInterface: TGUID read FVMInterface;
    property InstanceType: EInstanceType read FInstanceType;
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
  FVMInterface := AVMInterfaceID;
  FInstanceType:= AInstanceType;
end;

end.
