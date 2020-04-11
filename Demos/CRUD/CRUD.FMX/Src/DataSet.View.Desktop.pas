unit DataSet.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, Data.Bind.GenData, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.ObjectScope,
  FMX.Controls.Presentation, FMX.ScrollBox,
  Data.Bind.DBScope, Data.DB, Datasnap.DBClient,
  FMX.StdCtrls, FMX.Objects, FMX.Edit, FMX.Layouts,
  System.Actions, FMX.ActnList,
  FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox,
  Fmx.Bind.GenData,

  DataSet.Interfaces,

  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX;

type
  [View_For_ViewModel('PersonasMain', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmPersonasDesktop = class(TFormView<IDataSet_ViewModel>)
    ActionList1: TActionList;
    actGet: TAction;
    Layout1: TLayout;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    actNew: TAction;
    actDelete: TAction;
    actUpdate: TAction;
  private
    { Private declarations }
    procedure SetupView; override;
  public
    { Public declarations }
  end;

var
  frmPersonasDesktop: TfrmPersonasDesktop;

implementation

uses
  DataSet.ViewModel,

  MVVM.Types,
  Data.Bind.DBLinks;

{$R *.fmx}

procedure TfrmPersonasDesktop.SetupView;
begin
  ViewModel.TableName     := 'Personas';
  ViewModel.NewRowView    := 'New.Persona';
  ViewModel.UpdateRowView := 'Update.Persona';
  // actions
  actGet.Bind(procedure
              begin
                ViewModel.GetRows;
              end);
  actNew.Bind(procedure
              begin
                ViewModel.AppendRow;
              end);
  actUpdate.Bind(procedure
                 begin
                   ViewModel.UpdateActiveRow;
                 end);
  actDelete.Bind(procedure
                 begin
                   ViewModel.DeleteActiveRow;
                 end);

  // Dataset binding
  Binder.BindDataSet(TDataSet_ViewModel(ViewModel).DataSet, ListBox1);
end;

end.
