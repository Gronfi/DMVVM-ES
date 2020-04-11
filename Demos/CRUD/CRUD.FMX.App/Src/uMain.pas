unit uMain;

interface

uses
  System.Rtti, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.FMXUI.Wait, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FMX.Controls.Presentation, FMX.StdCtrls,
  FireDAC.Phys.SQLite, Data.DB, FireDAC.Comp.Client, FireDAC.Comp.UI;

type
  TfrmMain = class(TForm)
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    Button1: TButton;
    Button2: TButton;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure InitializateResources;
    procedure ConnectToBD;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Personas.Resources.Common,
  DataSet.Interfaces,
  DataSet.Model,
  DataSet.ViewModel,

  MVVM.Utils,
  MVVM.Core;

{$R *.fmx}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  VistaPersonas := MVVMCore.ViewsProvider.CreateView<IDataSet_ViewModel>(MVVMCore.DefaultViewPlatform, 'PersonasMain', nil, VistaModelo);
  Utils.ShowModalView(VistaPersonas, procedure (AResult: TModalResult)
                                     begin
                                       //
                                     end);
end;

procedure TfrmMain.ConnectToBD;
begin
  FDConnection1.Connected := True;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ConnectToBD;
  InitializateResources;
end;

procedure TfrmMain.InitializateResources;
begin
  Modelo := TDataSet_Model.Create;
  Modelo.Connection := FDConnection1;

  VistaModelo := MVVMCore.ViewModelProvider<IDataSet_ViewModel>;
  VistaModelo.SetModel(Modelo);
end;

end.
