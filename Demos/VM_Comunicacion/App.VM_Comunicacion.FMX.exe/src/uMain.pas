unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,

  MVVM.Interfaces.Architectural,
  Coche.Interfaces;

type
  TForm70 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageControl1: TImageControl;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    VistaCocheNew : IView<INewCoche_ViewModel>;
    VistaCocheUpdate : IView<IUpdateCoche_ViewModel>;
  end;

var
  Form70: TForm70;

implementation

uses
  Coche.Types,
  Coche.New.View.Desktop,

  MVVM.Services.Platform.FMX,
  MVVM.Utils,
  MVVM.Core;

{$R *.fmx}

procedure TForm70.Button1Click(Sender: TObject);
begin
  VistaCocheNew := MVVMCore.ViewsProvider.CreateView<INewCoche_ViewModel>(MVVMCore.DefaultViewPlatform, 'New.Coche', nil, nil);
  Utils.ShowModalView(VistaCocheNew, procedure (AResult: TModalResult)
                                     begin
                                       //
                                     end);
end;

procedure TForm70.Button2Click(Sender: TObject);
var
  LCoche: RCoche;
  LVM: IUpdateCoche_ViewModel;
begin
  LVM:= MVVMCore.IoC.ViewModelProvider<IUpdateCoche_ViewModel>;
  LCoche.ID     := 1;
  LCoche.Nombre := 'Dave';
  LCoche.Imagen := ImageControl1.Bitmap;
  LCoche.Dueño  := 10;
  LVM.Data      := LCoche;
  VistaCocheUpdate := MVVMCore.ViewsProvider.CreateView<IUpdateCoche_ViewModel>(MVVMCore.DefaultViewPlatform, 'Update.Coche', nil, LVM);
  Utils.ShowModalView(VistaCocheUpdate, procedure (AResult: TModalResult)
                                     begin
                                       //
                                     end);
end;

procedure TForm70.FormCreate(Sender: TObject);
begin
  InitializePlatform;

  MVVMCore.DefaultBindingStrategyName := 'LIVEBINDINGS';
  MVVMCore.DefaultViewPlatform        := 'WINDOWS_DESKTOP';

  MVVMCore.InitializationDone;
end;

end.
