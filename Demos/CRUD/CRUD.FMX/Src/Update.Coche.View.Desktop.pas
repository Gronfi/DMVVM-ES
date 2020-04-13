unit Update.Coche.View.Desktop;

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
  [View_For_ViewModel('Update.Coche', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmUpdateCoche = class(TFormView<IDataSet_ViewModel>)
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
  public
    { Public declarations }
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

procedure TfrmUpdateCoche.SetupView;
var
  LDataToGet: TFieldsToGet;
  LData     : TFieldConverters;
  I         : Integer;
begin
  //get data to update
  LDataToGet.AddData('ID');
  LDataToGet.AddData('NOMBRE');
  LDataToGet.AddData('IMAGEN', True);

  LData := ViewModel.GetRows(LDataToGet);

  for I := Low(LData) to High(LData) do
  begin
    case Utils.StringToCaseSelect(LData[I].FieldName, ['ID', 'NOMBRE', 'IMAGEN']) of
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
          ImageControl1.Bitmap := LData[I].FieldValue.AsType<TBitmap>;
        end;
    end;
  end;
  //binding
  actUpdate.Bind(procedure
                 var
                   LDatos: TFieldConverters;
                 begin
                   LDatos.AddData('NOMBRE', edtNombre.Text);
                   LDatos.AddData('IMAGEN', TValue.From<TBitmap>(ImageControl1.Bitmap));
                   ViewModel.UpdateActiveRow(LDatos);
                 end
                );
end;

initialization

TfrmUpdateCoche.ClassName;

end.
