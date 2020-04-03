unit uView;

interface

uses
  FMX.Forms,
  System.Generics.Collections,

  uInterfaces;

type
  TFormView<I: IViewModel; K: TViewModel> = class abstract(TForm, IView, IVM_User<I, K>)
  protected
    var
      FViewModel : IViewModel;

    function GetVM_AsInterface: I; virtual; abstract;
    function GetVM_AsObject: K; virtual; abstract;

    procedure Loaded; override;

    procedure CreateVM;

  public

    property VM_AsInterface: I read GetVM_AsInterface;

    property VM_AsObject: K read GetVM_AsObject;
  end;

  TFrameView<I: IViewModel; K: TViewModel> = class abstract(TFrame, IView, IVM_User<I, K>)
  protected
    var
      FViewModel : IViewModel;

    function GetVM_AsInterface: I; virtual; abstract;
    function GetVM_AsObject: K; virtual; abstract;

    procedure Loaded; override;

    procedure CreateVM;

  public
    property VM_AsInterface: I read GetVM_AsInterface;

    property VM_AsObject: K read GetVM_AsObject;
  end;

var
  FSingletonViewModels: TDictionary<TGUID, IViewModel>;

implementation

uses
  System.RTTI,

  uTypes,
  uAtributos;

{ TFormView<I, K> }

procedure TFormView<I, K>.CreateVM;
var
  Ctx  : TRttiContext;
  Typ  : TRttiType;
  LAttr: TCustomAttribute;
  LVMA : ViewForVM;
begin
  if Assigned(FViewModel) then Exit;

  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Self.ClassInfo);
    // Loop for attributes
    for LAttr in Typ.GetAttributes do
    begin
      //ViewForVM
      if LAttr is ViewForVM then
      begin
        LVMA       := LAttr as ViewForVM;
        case LVMA.InstanceType of
          EInstanceType.itSingleton:
            begin
              if not FSingletonViewModels.TryGetValue(LVMA.VMInterface, FViewModel) then
              begin
                FViewModel := LVMA.VMClass.Create;
                FSingletonViewModels.AddOrSetValue(LVMA.VMInterface, FViewModel);
              end;
            end;
          EInstanceType.itDefault:
            begin
              FViewModel := LVMA.VMClass.Create;
            end;
        end;
        Exit;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

procedure TFormView<I, K>.Loaded;
begin
  inherited;
  CreateVM;
end;

{ TFrameView<I, K> }

procedure TFrameView<I, K>.CreateVM;
var
  Ctx  : TRttiContext;
  Typ  : TRttiType;
  LAttr: TCustomAttribute;
  LVMA : ViewForVM;
begin
  if Assigned(FViewModel) then Exit;

  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Self.ClassInfo);
    // Loop for attributes
    for LAttr in Typ.GetAttributes do
    begin
      //ViewForVM
      if LAttr is ViewForVM then
      begin
        LVMA       := LAttr as ViewForVM;
        case LVMA.InstanceType of
          EInstanceType.itSingleton:
            begin
              if not FSingletonViewModels.TryGetValue(LVMA.VMInterface, FViewModel) then
              begin
                FViewModel := LVMA.VMClass.Create;
                FSingletonViewModels.AddOrSetValue(LVMA.VMInterface, FViewModel);
              end;
            end;
          EInstanceType.itDefault:
            begin
              FViewModel := LVMA.VMClass.Create;
            end;
        end;
        Exit;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

procedure TFrameView<I, K>.Loaded;
begin
  inherited;
  CreateVM;
end;

initialization
  FSingletonViewModels := TDictionary<TGUID, IViewModel>.Create;

finalization
  FSingletonViewModels.Free;

end.
