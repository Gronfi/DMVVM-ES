unit Coches.View.Desktop.ListBox;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls, System.Actions,
  FMX.ActnList, FMX.Objects,

  DataSet.Interfaces,

  MVVM.Attributes,
  MVVM.Interfaces, MVVM.Bindings,
  MVVM.Views.Platform.FMX,
  MVVM.Controls.Platform.FMX;

type
  [View_For_ViewModel('CochesMain.ListBox', IDataSet_ViewModel, 'WINDOWS_DESKTOP')]
  TfrmCochesMainLB = class(TFormView<IDataSet_ViewModel>)
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
    ListBox1: TListBox;
    StyleBook1: TStyleBook;
  private
    { Private declarations }
  protected
    procedure SetupView; override;
  public
    { Public declarations }
  end;

var
  frmCochesMainLB: TfrmCochesMainLB;

implementation

uses
  MVVM.Utils,
  MVVM.Types;

{$R *.fmx}

{ TfrmCochesMainLB }

procedure TfrmCochesMainLB.SetupView;
begin
  inherited;
  // dataset configuration
  ViewModel.TableName     := 'Coches';
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

  ListBox1.DefaultItemStyles.ItemStyle := 'CustomItem';
  ListBox1.ItemHeight                  := 64;

  Binder.BindDataSetToListBox(ViewModel.DataSet, ListBox1, [
                                                       TListBoxConversionData.Create('ID', 'Text'),
                                                       TListBoxConversionData.Create('NOMBRE', Utils.StyledFieldOfComponent('resolution')),
                                                       TListBoxConversionData.Create('IMAGEN', 'Bitmap')
                                                     ], False);
end;

initialization

TfrmCochesMainLB.ClassName;

end.
