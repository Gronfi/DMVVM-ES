unit uMain;

interface

uses
  System.Rtti, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.FMXUI.Wait, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FMX.Controls.Presentation, FMX.StdCtrls,
  FireDAC.Phys.SQLite, Data.DB, FireDAC.Comp.Client, FireDAC.Comp.UI,
  FMX.Objects, FMX.Effects, FMX.Layouts, FireDAC.Phys.SQLiteWrapper.Stat;

type
  TfrmMain = class(TForm)
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    Button1: TButton;
    Button2: TButton;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Rectangle1: TRectangle;
    Text3: TText;
    ShadowEffect1: TShadowEffect;
    Layout1: TLayout;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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
  CRUD.Resources.Common,
  DataSet.Interfaces,
  DataSet.Model,
  DataSet.ViewModel,

  MVVM.Utils,
  MVVM.Core;

{$R *.fmx}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  VistaTabla := MVVMCore.ViewsProvider.CreateView<IDataSet_ViewModel>(MVVMCore.DefaultViewPlatform, 'PersonasMain', nil, VistaModelo);
  Utils.ShowModalView(VistaTabla, procedure (AResult: TModalResult)
                                     begin
                                       //
                                     end);
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  VistaTabla := MVVMCore.ViewsProvider.CreateView<IDataSet_ViewModel>(MVVMCore.DefaultViewPlatform, 'CochesMain.Grid', nil, VistaModelo);
  Utils.ShowModalView(VistaTabla, procedure (AResult: TModalResult)
                                     begin
                                       //
                                     end);
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  VistaTabla := MVVMCore.ViewsProvider.CreateView<IDataSet_ViewModel>(MVVMCore.DefaultViewPlatform, 'CochesMain.ListBox', nil, VistaModelo);
  Utils.ShowModalView(VistaTabla, procedure (AResult: TModalResult)
                                     begin
                                       //
                                     end);
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  VistaTabla := MVVMCore.ViewsProvider.CreateView<IDataSet_ViewModel>(MVVMCore.DefaultViewPlatform, 'CochesMain.ListView', nil, VistaModelo);
  Utils.ShowModalView(VistaTabla, procedure (AResult: TModalResult)
                                     begin
                                       //
                                     end);
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  FDConnection1.Connected := not FDConnection1.Connected
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

  VistaModelo := MVVMCore.IoC.ViewModelProvider<IDataSet_ViewModel>;
  VistaModelo.SetModel(Modelo);
end;

end.
