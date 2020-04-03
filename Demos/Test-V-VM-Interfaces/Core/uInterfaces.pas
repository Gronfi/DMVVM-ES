unit uInterfaces;

interface

type
  IViewModel = interface
    ['{68989DD0-5306-42CE-B90D-852B8BA6CC07}']
  end;

  TViewModel = class(TInterfacedObject, IViewModel)
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TViewModelClass = class of TViewModel;

  IVM_User<I: IViewModel; K: TViewModel> = interface
    ['{E70146E0-464C-4604-8602-66B1702C3341}']
    function GetVM_AsInterface: I;
    function GetVM_AsObject: K;

    property VM_AsInterface: I read GetVM_AsInterface;
    property VM_AsObject: K read GetVM_AsObject;
  end;

  IView = interface
    ['{AFCC9C0D-321F-4197-8FC0-6343B43FCF62}']
  end;

implementation

{ TViewModel }

constructor TViewModel.Create;
begin
  inherited;
end;

destructor TViewModel.Destroy;
begin
  inherited;
end;

end.
