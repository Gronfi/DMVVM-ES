unit Coches.View.Desktop.ListView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,

  //Fmx.Bind.Navigator,

  System.Actions, FMX.ActnList,

  Fmx.Bind.Editors, Fmx.Bind.DBEngExt,

  Data.Bind.EngExt,

  //
  System.Bindings.Expression, System.Bindings.Helper, System.Bindings.Outputs,
  Data.Bind.Components, Data.Bind.DBScope, Data.Bind.Controls,
  //


  DataSet.Interfaces,

  MVVM.Attributes,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Views.Platform.FMX,
  MVVM.Controls.Platform.FMX;

type
  [View_For_ViewModel('CochesMain.ListView', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmViewCochesLV = class(TFormView<IDataSet_ViewModel>)
    actlst1: TActionList;
    actGet: TAction;
    actNew: TAction;
    actDelete: TAction;
    actUpdate: TAction;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ListView1: TListView;
  private
    { Private declarations }
  protected
    procedure SetupView; override;
  public
    { Public declarations }
  end;

var
  frmViewCochesLV: TfrmViewCochesLV;

implementation

uses
  MVVM.Utils,
  MVVM.Types;

{$R *.fmx}

{ TfrmViewCochesLV }

procedure TfrmViewCochesLV.SetupView;
begin
  inherited;
  // dataset configuration
  ViewModel.TableName     := 'Coches';
  ViewModel.TableIndex    := 'ID';
  // views configuration
  ViewModel.NewRowView    := 'New.Coche';
  ViewModel.UpdateRowView := 'Update.Coche';
  // actions binding
  actGet.Bind(ViewModel.DoMakeGetRows);
  actNew.Bind(ViewModel.DoMakeAppend, ViewModel.IsOpen);
  actUpdate.Bind(ViewModel.DoMakeUpdate, ViewModel.IsOpen);
  actDelete.Bind(ViewModel.DoDeleteActiveRow, ViewModel.IsOpen);
  // Dataset binding
  ViewModel.DoMakeGetRows; //open the dataset and get rows

  Binder.BindDataSetToListView(ViewModel.DataSet, ListView1,
                               [
                                 TListViewConversionData.Create('ID', 'Item.Text', '', True),
                                 TListViewConversionData.Create('NOMBRE', 'Item.Detail'),
                                 TListViewConversionData.Create('IMAGEN', 'Item.Bitmap')
                               ],
                               False);
end;

initialization

TfrmViewCochesLV.ClassName;

end.
