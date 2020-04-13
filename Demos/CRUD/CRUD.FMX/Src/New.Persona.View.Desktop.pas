unit New.Persona.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  System.Actions, FMX.ActnList, FMX.StdActns,

  DataSet.Interfaces,

  MVVM.Attributes,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX;

type
  [View_For_ViewModel('New.Persona', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmNewPersona = class(TFormView<IDataSet_ViewModel>)
    Rectangle1: TRectangle;
    Text1: TText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtNombre: TEdit;
    edtApellidos: TEdit;
    edtFechaNacimiento: TEdit;
    edtPeso: TEdit;
    Button1: TButton;
    Button2: TButton;
    actlst1: TActionList;
    actAdd: TAction;
  protected
    { Private declarations }
    procedure SetupView; override;
  public
    { Public declarations }
  end;

var
  frmNewPersona: TfrmNewPersona;

implementation

uses
  DataSet.Types;

{$R *.fmx}

procedure TfrmNewPersona.SetupView;
begin
  actAdd.Bind(procedure
              var
                LDatosPersona: TFieldConverters;
              begin
                LDatosPersona.AddData('NOMBRE', edtNombre.Text);
                LDatosPersona.AddData('APELLIDOS', edtApellidos.Text);
                LDatosPersona.AddData('FECHA_NACIMIENTO', edtFechaNacimiento.Text);
                LDatosPersona.AddData('PESO', StrToInt(edtPeso.Text));
                ViewModel.AppendRow(LDatosPersona);
              end
             );
end;

initialization
  TfrmNewPersona.ClassName

end.
