unit Personas.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
   System.Rtti,
  //Data.Bind.GenData, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox,
  //Data.Bind.DBScope, Data.DB,
  //Datasnap.DBClient, Data.Bind.DBLinks,
  FMX.StdCtrls, FMX.Objects, FMX.Edit, FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox,
  FMX.Grid, FMX.Grid.Style,

  System.Actions, FMX.ActnList,

  Data.DB,

  (*
  System.Bindings.Outputs,
  Fmx.Bind.GenData,
  Fmx.Bind.Editors,
  Fmx.Bind.DBEngExt,
  Fmx.Bind.Navigator,
  Fmx.Bind.Grid,

  Data.Bind.GenData,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  Data.Bind.EngExt,
  Data.Bind.DBScope,
  Data.Bind.Controls,
  Data.Bind.Grid,
  *)

  DataSet.Interfaces,

  MVVM.Attributes,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX;

type
  [View_For_ViewModel('PersonasMain', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmPersonasDesktop = class(TFormView<IDataSet_ViewModel>)
    ActionList1: TActionList;
    actGet: TAction;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    actNew: TAction;
    actDelete: TAction;
    actUpdate: TAction;
    Grid1: TGrid;
    Button5: TButton;
    Button6: TButton;
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  protected
    { Private declarations }
    procedure SetupView; override;
  public
    { Public declarations }
    //function ClearDataSetBindingFromGrid(AGrid: TCustomGrid): Boolean;
    //procedure BindDataSetToGrid(ADataSet: TDataSet; AGrid: TCustomGrid);
  end;

var
  frmPersonasDesktop: TfrmPersonasDesktop;

implementation

uses
  DataSet.ViewModel,

  MVVM.Core,
  MVVM.Types;

{$R *.fmx}

(*
function TfrmPersonasDesktop.ClearDataSetBindingFromGrid(AGrid: TCustomGrid): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AGrid.ComponentCount - 1 do
  begin
    if (AGrid.Components[I] is TBindSourceDB) then
    begin
      AGrid.Components[I].Free;
      Exit(True);
    end;
  end;
end;

procedure TfrmPersonasDesktop.BindDataSetToGrid(ADataSet: TDataSet;
  AGrid: TCustomGrid);
var
  LLinker: TLinkGridToDataSource;
  LSource: TBindSourceDB;
begin
  ClearDataSetBindingFromGrid(AGrid);

  LSource          := TBindSourceDB.Create(AGrid);
  LSource.DataSet  := ADataSet;
  LLinker          := TLinkGridToDataSource.Create(LSource);
  LLinker.Category := 'Quick Bindings';
  LLinker.GridControl  := AGrid;

  LLinker.DataSource := LSource;
  LLinker.Active   := True;
end;
*)

procedure TfrmPersonasDesktop.Button5Click(Sender: TObject);
var
  LFields: TFieldConverters;
begin
  SetLength(LFields, 1);
  LFields[0].FieldName  := 'NOMBRE';
  LFields[0].FieldValue := 'PACO';
  ViewModel.UpdateActiveRow(LFields);
end;

procedure TfrmPersonasDesktop.Button6Click(Sender: TObject);
var
  LFields: TFieldConverters;
begin
  SetLength(LFields, 4);
  LFields[0].FieldName  := 'NOMBRE';
  LFields[0].FieldValue := 'ALBERTO';
  LFields[1].FieldName  := 'APELLIDOS';
  LFields[1].FieldValue := 'ALCELAY ELORRIAGA';
  LFields[2].FieldName  := 'FECHA_NACIMIENTO';
  LFields[2].FieldValue := '1968/11/06';
  LFields[3].FieldName  := 'PESO';
  LFields[3].FieldValue := 85;
  ViewModel.AppendRow(LFields);
end;

procedure TfrmPersonasDesktop.SetupView;
begin
  // dataset configuration
  ViewModel.TableName     := 'Personas';
  // views configuration
  ViewModel.NewRowView    := 'New.Persona';
  ViewModel.UpdateRowView := 'Update.Persona';
  // actions binding
  actGet.Bind(procedure
              begin
                ViewModel.GetRows;
              end);
  actNew.Bind(procedure
              begin
                ViewModel.MakeAppend;
              end);
  actUpdate.Bind(procedure
                 begin
                   ViewModel.MakeUpdate;
                 end);
  actDelete.Bind(procedure
                 begin
                   ViewModel.DeleteActiveRow;
                 end);

  // Dataset binding
  ViewModel.GetRows;
  MVVMCore.PlatformServices.MessageDlg('Rows: ' + ViewModel.DataSet.RecordCount.ToString, 'Hola');

  //BindDataSetToGrid(ViewModel.DataSet, Grid1);
  Binder.BindDataSetToGrid(ViewModel.DataSet, Grid1);
end;

initialization
  TfrmPersonasDesktop.ClassName;

end.
