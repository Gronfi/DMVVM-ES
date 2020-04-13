unit Update.Persona.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Objects,
  System.Actions, FMX.ActnList,

  DataSet.Interfaces,

  MVVM.Attributes,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX;

type
  [View_For_ViewModel('Update.Persona', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmUpdatePersona = class(TFormView<IDataSet_ViewModel>)
    Rectangle1: TRectangle;
    Text3: TText;
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
    Label5: TLabel;
    edtID: TEdit;
    ActionList1: TActionList;
    actUpdate: TAction;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetupView; override;
  end;

implementation

uses
  MVVM.Utils,

  DataSet.Types;

{$R *.fmx}

{ TfrmUpdatePersona }

procedure TfrmUpdatePersona.SetupView;
var
  LDataToGet: TFieldsToGet;
  LData     : TFieldConverters;
  I         : Integer;
begin
  //get data to update
  LDataToGet.AddData('ID');
  LDataToGet.AddData('NOMBRE');
  LDataToGet.AddData('APELLIDOS');
  LDataToGet.AddData('FECHA_NACIMIENTO');
  LDataToGet.AddData('PESO');

  LData := ViewModel.GetRows(LDataToGet);

  for I := Low(LData) to High(LData) do
  begin
    case Utils.StringToCaseSelect(LData[I].FieldName, ['ID', 'NOMBRE', 'APELLIDOS', 'FECHA_NACIMIENTO', 'PESO']) of
      0:
        begin
          edtID.Text := LData[I].FieldValue.AsInteger.ToString;
        end;
      1:
        begin
          edtNombre.Text := LData[I].FieldValue.AsString;
        end;
      2:
        begin
          edtApellidos.Text := LData[I].FieldValue.AsString;
        end;
      3:
        begin
          edtFechaNacimiento.Text := LData[I].FieldValue.AsString;
        end;
      4:
        begin
          edtPeso.Text := LData[I].FieldValue.AsInteger.ToString;
        end;
    end;
  end;
  //binding
  actUpdate.Bind(procedure
                 var
                   LDatosPersona: TFieldConverters;
                 begin
                   LDatosPersona.AddData('NOMBRE', edtNombre.Text);
                   LDatosPersona.AddData('APELLIDOS', edtApellidos.Text);
                   LDatosPersona.AddData('FECHA_NACIMIENTO', edtFechaNacimiento.Text);
                   LDatosPersona.AddData('PESO', StrToInt(edtPeso.Text));
                   ViewModel.UpdateActiveRow(LDatosPersona);
                 end
                );
end;

initialization

TfrmUpdatePersona.ClassName

end.
