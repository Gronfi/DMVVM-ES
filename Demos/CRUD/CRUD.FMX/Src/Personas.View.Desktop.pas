unit Personas.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.StdCtrls, FMX.Objects, FMX.Edit, FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox,
  FMX.Grid, FMX.Grid.Style,

  System.Actions, FMX.ActnList,

  DataSet.Interfaces,

  MVVM.Attributes,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX;

type
  [View_For_ViewModel('PersonasMain', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmPersonasDesktop = class(TFormView<IDataSet_ViewModel>)
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Grid1: TGrid;
    ActionList1: TActionList;
    [Command(GET_ROWS)]
    actGet: TAction;
    [Command(APPEND_ROW, DATASET_IS_OPEN)]
    actNew: TAction;
    [Command(DELETE_ROW, DATASET_IS_OPEN)]
    actDelete: TAction;
    [Command(UPDATE_ROW, DATASET_IS_OPEN)]
    actUpdate: TAction;
    Button5: TButton;
    Button6: TButton;
    [Command(CLOSE_DATASET, DATASET_IS_OPEN)]
    actCloseDataSet: TAction;
    [Command(OPEN_DATASET, DATASET_IS_CLOSED)]
    actOpenDataSet: TAction;
  protected
    { Private declarations }
    procedure SetupView; override;
  public
    { Public declarations }
  end;

implementation

uses
  DataSet.ViewModel,

  MVVM.Core,
  MVVM.Types;

{$R *.fmx}

procedure TfrmPersonasDesktop.SetupView;
begin
  inherited;
  // dataset configuration
  ViewModel.TableName     := 'Personas';
  // views configuration
  ViewModel.NewRowView    := 'New.Persona';
  ViewModel.UpdateRowView := 'Update.Persona';
  // actions binding
  //actGet.Bind(ViewModel.DoMakeGetRows);
  //actNew.Bind(ViewModel.DoMakeAppend, ViewModel.IsOpen);
  //actUpdate.Bind(ViewModel.DoMakeUpdate, ViewModel.IsOpen);
  //actDelete.Bind(ViewModel.DoDeleteActiveRow, ViewModel.IsOpen);

  // Dataset binding
  ViewModel.DoMakeGetRows; //open the dataset and get rows

  Binder.BindDataSetToGrid(ViewModel.DataSet, Grid1);
end;

initialization
  TfrmPersonasDesktop.ClassName;

end.
