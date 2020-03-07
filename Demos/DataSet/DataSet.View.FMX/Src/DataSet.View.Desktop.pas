unit DataSet.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, Data.Bind.GenData, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.ObjectScope,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,

  MVVM.Interfaces, MVVM.Bindings,
  DataSet.Interfaces, Data.Bind.DBScope, Data.DB, Datasnap.DBClient,
  FMX.StdCtrls, FMX.Objects, FMX.Edit;

type
  TfrmDataSetDesktop = class(TForm, IDataSetFile_View)
    Grid1: TGrid;
    BindingsList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    Button1: TButton;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
    FViewModel: IDataSetFile_ViewModel;
    FBinder   : TBindingHelper_V2;
  public
    { Public declarations }
    procedure RefreshData;
    procedure AddViewModel(AViewModel: IViewModel<IDataSetFile_Model>);
    procedure RemoveViewModel(AViewModel: IViewModel<IDataSetFile_Model>);
  end;

var
  frmDataSetDesktop: TfrmDataSetDesktop;

implementation

uses
  MVVM.Types,
  Data.Bind.DBLinks;

{$R *.fmx}

procedure TfrmDataSetDesktop.FormCreate(Sender: TObject);
begin
  FBinder := TBindingHelper_V2.Create(Self);
end;

procedure TfrmDataSetDesktop.AddViewModel(AViewModel: IViewModel<IDataSetFile_Model>);
begin
  if FViewModel <> AViewModel then
  begin
    if Supports(AViewModel, IDataSetFile_ViewModel, FViewModel)  then
    begin
      FViewModel := AViewModel as IDataSetFile_ViewModel;
      //Bindings a capela
      FBinder.Bind(Edit1, 'Text', TObject(FViewModel), 'FileName', EBindDirection.TwoWay, [EBindFlag.DontApply]);
      RefreshData;
    end
    else raise Exception.Create('No casan las interfaces');
  end;
end;

procedure TfrmDataSetDesktop.Button1Click(Sender: TObject);
begin
  RefreshData;
end;

procedure TfrmDataSetDesktop.Edit1Change(Sender: TObject);
begin
  FBinder.Notify(Edit1, 'Text');
end;

procedure TfrmDataSetDesktop.RefreshData;
begin
  //Cerramos
  if FViewModel.DataSet.Active then
    FViewModel.DataSet.Active := False;
  //Abrimoa
  if not FViewModel.DataSet.Active then
  begin
    FViewModel.AbrirDataSet;
    //hacemos binding a grid
    BindSourceDB1.DataSet := FViewModel.DataSet;
  end;
end;

procedure TfrmDataSetDesktop.RemoveViewModel(AViewModel: IViewModel<IDataSetFile_Model>);
begin
  FViewModel := nil;
end;

end.
