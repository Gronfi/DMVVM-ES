program Test_V_VMs;

uses
  System.StartUpCopy,
  FMX.Forms,
  uDemoViewTipo1 in 'Demo\uDemoViewTipo1.pas' {frmIntf1},
  uInterfaces in 'Core\uInterfaces.pas',
  uAtributos in 'Core\uAtributos.pas',
  uTypes in 'Core\uTypes.pas',
  uDemoViewModels in 'Demo\uDemoViewModels.pas',
  uDemoInterfaces in 'Demo\uDemoInterfaces.pas',
  uView in 'Core\uView.pas',
  uDemoViewTipo2 in 'Demo\uDemoViewTipo2.pas' {frmIntf2},
  uDemoViewFrameTipo1 in 'Demo\uDemoViewFrameTipo1.pas' {Frame1: TFrame},
  uDemoViewFrameTipo2 in 'Demo\uDemoViewFrameTipo2.pas' {Frame2: TFrame},
  uDemoViewTipo3 in 'Demo\uDemoViewTipo3.pas' {frmMultiVista};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmIntf1, frmIntf1);
  Application.CreateForm(TfrmMultiVista, frmMultiVista);
  Application.Run;
end.
