unit uAtributos;

interface

uses
  uInterfaces, uTypes;

type
  TMVVMCustomAttribute = class(TCustomAttribute)
  end;

  ViewForVM = class(TMVVMCustomAttribute)
  strict private
    FVMInterface: TGUID;
    FVMClass: TViewModelClass;
    FInstanceType: EInstanceType;
  public
    constructor Create(AInterfaceID: TGUID; AVMClassType: TViewModelClass; const AInstanceType: EInstanceType = EInstanceType.itDefault);
    property VMInterface: TGUID read FVMInterface;
    property VMClass: TViewModelClass read FVMClass;
    property InstanceType: EInstanceType read FInstanceType;
  end;

implementation

{ ViewForVM }

constructor ViewForVM.Create(AInterfaceID: TGUID; AVMClassType: TViewModelClass; const AInstanceType: EInstanceType);
begin
  FVMInterface := AInterfaceID;
  FVMClass     := AVMClassType;
  FInstanceType:= AInstanceType;
end;

end.
