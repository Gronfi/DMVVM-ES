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
  Data.Bind.DBLinks,
*)

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
    actGet: TAction;
    actNew: TAction;
    actDelete: TAction;
    actUpdate: TAction;
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
  // dataset configuration
  ViewModel.TableName     := 'Personas';
  // views configuration
  ViewModel.NewRowView    := 'New.Persona';
  ViewModel.UpdateRowView := 'Update.Persona';
  // actions binding
  actGet.Bind(ViewModel.DoMakeGetRows);
  actNew.Bind(ViewModel.DoMakeAppend, ViewModel.IsOpen);
  actUpdate.Bind(ViewModel.DoMakeUpdate, ViewModel.IsOpen);
  actDelete.Bind(ViewModel.DoDeleteActiveRow, ViewModel.IsOpen);

  // Dataset binding
  ViewModel.DoMakeGetRows; //open the dataset and get rows

  Binder.BindDataSetToGrid(ViewModel.DataSet, Grid1);
end;

initialization
  TfrmPersonasDesktop.ClassName;

end.
