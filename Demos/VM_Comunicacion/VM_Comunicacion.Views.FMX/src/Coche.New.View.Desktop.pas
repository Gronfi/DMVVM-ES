unit Coche.New.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation,
  System.Actions, FMX.ActnList,

  Coche.Interfaces,
  Coche.Types,

  MVVM.Attributes,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Controls.Platform.FMX,
  MVVM.Types,
  MVVM.Views.Platform.FMX;

type
  [View_For_ViewModel('New.Coche', INewCoche_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmNewCoche = class(TFormView<INewCoche_ViewModel>)
    Button1: TButton;
    Button2: TButton;
    edtNombre: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Rectangle1: TRectangle;
    Text1: TText;
    ImageControl1: TImageControl;
    ActionList1: TActionList;
    actAdd: TAction;
  protected
    { Private declarations }
    procedure SetupView; override;

    function _GetData: RCoche;
    function GetData: TParam<RCoche>;
  public
    { Public declarations }

    property Data: TParam<RCoche> read GetData;
  end;

implementation

uses
  System.Rtti;

{$R *.fmx}

{ TfrmNewCoche }

function TfrmNewCoche.GetData: TParam<RCoche>;
begin
  Result := _GetData;
end;

procedure TfrmNewCoche.SetupView;
begin
  actAdd.Bind<RCoche>(ViewModel.DoSetData, Data);
end;

function TfrmNewCoche._GetData: RCoche;
begin
  Result.ID := 0;
  Result.Nombre := edtNombre.Text;
  Result.Imagen := ImageControl1.Bitmap;
  Result.Dueño  := 0;
end;

initialization

TfrmNewCoche.ClassName;

end.
