unit New.Coche.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation,
  System.Actions, FMX.ActnList,

  DataSet.Interfaces,

  MVVM.Attributes,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX;

type
  [View_For_ViewModel('New.Coche', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmNewCoche = class(TFormView<IDataSet_ViewModel>)
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
  public
    { Public declarations }
  end;

implementation

uses
  System.Rtti,

  DataSet.Types;

{$R *.fmx}

{ TfrmNewCoche }

procedure TfrmNewCoche.SetupView;
begin
  actAdd.Bind(procedure
              var
                LDatosPersona: TFieldConverters;
              begin
                LDatosPersona.AddData('NOMBRE', edtNombre.Text);
                LDatosPersona.AddData('IMAGEN', TValue.From<TBitmap>(ImageControl1.Bitmap));
                ViewModel.AppendRow(LDatosPersona);
              end
             );
end;

initialization

TfrmNewCoche.ClassName;

end.
