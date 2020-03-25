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
  TfrmDataSetDesktop = class(TFormView<IDataSetFile_ViewModel>, IDataSetFile_View)
    Button1: TButton;
    edtFileName: TEdit;
    ActionList1: TActionList;
    actRefresco: TAction;
    Layout1: TLayout;
    lbStatus: TLabel;
    ListBox1: TListBox;
    procedure actRefrescoExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure edtFileNameChange(Sender: TObject);
  private
    { Private declarations }
    procedure SetupView; override;
  public
    { Public declarations }
    procedure RefreshData;
  end;

var
  frmDataSetDesktop: TfrmDataSetDesktop;

implementation

uses
  DataSet.ViewModel,

  MVVM.Types,
  Data.Bind.DBLinks;

{$R *.fmx}

procedure TfrmDataSetDesktop.actRefrescoExecute(Sender: TObject);
begin
  RefreshData;
end;

procedure TfrmDataSetDesktop.Button1Click(Sender: TObject);
begin
  RefreshData;
end;

procedure TfrmDataSetDesktop.edtFileNameChange(Sender: TObject);
begin
  //FBinder.Notify(Edit1, 'Text');
end;

procedure TfrmDataSetDesktop.RefreshData;
begin
  //Cerramos
  (*
  if FViewModel.DataSet.Active then
    FViewModel.DataSet.Active := False;
  //Abrimoa
  if not FViewModel.DataSet.Active then
  begin
    FViewModel.AbrirDataSet;
    //hacemos binding a grid
    BindSourceDB1.DataSet := FViewModel.DataSet;
  end;
  *)
end;

procedure TfrmDataSetDesktop.SetupView;
begin
  // Refresh Data
  actRefresco.Bind(TDataSet_ViewModel(ViewModel).AbrirDataSet, TDataSet_ViewModel(ViewModel).GetIsValidFile);
  Binder.BindAction(actRefresco);
  // Filename binding
  Binder.Bind(TDataSet_ViewModel(ViewModel), 'FileName', edtFileName, 'Text', EBindDirection.OneWay);
  // Dataset binding
  Binder.BindDataSet(TDataSet_ViewModel(ViewModel).DataSet, ListBox1);
end;

end.
