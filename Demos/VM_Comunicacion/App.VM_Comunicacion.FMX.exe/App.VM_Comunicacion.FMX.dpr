program App.VM_Comunicacion.FMX;

uses
  System.StartUpCopy,
  MVVM.Bindings.LiveBindings,
  MVVM.Binding.LiveBindings.Controls.FMX,
  FMX.Forms,
  uMain in 'src\uMain.pas' {Form70};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm70, Form70);
  Application.Run;
end.
