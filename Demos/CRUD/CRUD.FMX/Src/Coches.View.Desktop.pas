unit Coches.View.Desktop;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Grid, System.Actions, FMX.ActnList,

  DataSet.Interfaces,

  MVVM.Attributes,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX, FMX.Objects;

type
  [View_For_ViewModel('CochesMain.Grid', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmCochesDesktop = class(TFormView<IDataSet_ViewModel>)
    actlst1: TActionList;
    actGet: TAction;
    actNew: TAction;
    actDelete: TAction;
    actUpdate: TAction;
    Grid1: TGrid;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Image1: TImage;
  protected
    { Private declarations }
    procedure SetupView; override;
  public
    { Public declarations }
  end;

implementation

uses
  MVVM.Types;

{$R *.fmx}

{ TForm2 }

procedure TfrmCochesDesktop.SetupView;
begin
  // dataset configuration
  ViewModel.TableName     := 'Coches';
  // views configuration
  ViewModel.NewRowView    := 'New.Coche';
  ViewModel.UpdateRowView := 'Update.Coche';
  // actions binding
  actGet.Bind(procedure
              begin
                ViewModel.MakeGetRows;
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
  ViewModel.MakeGetRows;

  Binder.BindDataSetToGrid(ViewModel.DataSet, Grid1, [
                                                       TGridColumnTemplate.Create('ID', 'ID', True, 50, '', '', ''),
                                                       TGridColumnTemplate.Create('NOMBRE', 'NOMBRE', True, 150, '', '', '')
                                                     ]);
  Binder.BindDataSetFieldToProperty(ViewModel.DataSet, 'IMAGEN', Image1, 'Bitmap');
end;

initialization

TfrmCochesDesktop.ClassName;

end.
