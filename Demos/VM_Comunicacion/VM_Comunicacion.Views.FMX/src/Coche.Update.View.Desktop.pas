unit Coche.Update.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation,
  System.Actions, FMX.ActnList,

  Coche.Interfaces,
  Coche.Types,

  MVVM.Attributes,
  MVVM.Types,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX;

type
  [View_For_ViewModel('Update.Coche', IUpdateCoche_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmUpdateCoche = class(TFormView<IUpdateCoche_ViewModel>)
    Button1: TButton;
    Button2: TButton;
    edtID: TEdit;
    edtNombre: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Rectangle1: TRectangle;
    Text3: TText;
    ImageControl1: TImageControl;
    ActionList1: TActionList;
    actUpdate: TAction;
  protected
    { Private declarations }
    procedure SetupView; override;

    function _GetData: RCoche;
    function GetData: TParam<RCoche>;
  public
    { Public declarations }
    property Data: TParam<RCoche> read GetData;
  end;

var
  frmUpdateCoche: TfrmUpdateCoche;

implementation

uses
  System.Rtti,

  MVVM.Utils,

  DataSet.Types;

{$R *.fmx}

{ TfrmUpdateCoche }

function TfrmUpdateCoche.GetData: TParam<RCoche>;
begin
  Result := _GetData;
end;

procedure TfrmUpdateCoche.SetupView;
begin
  //get data to update
  edtID.Text           := ViewModel.Data.ID.ToString;
  edtNombre.Text       := ViewModel.Data.Nombre;
  ImageControl1.Bitmap := ViewModel.Data.Imagen as TBitmap;

  //binding
  actUpdate.Bind<RCoche>(ViewModel.DoSetData, Data);
end;

function TfrmUpdateCoche._GetData: RCoche;
begin
  Result.ID     := StrToInt(edtID.Text);
  Result.Nombre := edtNombre.Text;
  Result.Imagen := ImageControl1.Bitmap;
  Result.Dueño  := 0;
end;

initialization

TfrmUpdateCoche.ClassName;

end.
