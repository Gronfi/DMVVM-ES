unit MVVM.Servicios.Platform.VCL;

interface

uses
  MVVM.Core;

type
  TVCLServicioDialogo = class(TServicioDialogoBase)
  public
    function MessageDlg(const ATitulo: string; const ATexto: String): Boolean; override;
  end;

  procedure InitializePlatform;

implementation

uses
  VCL.Dialogs,
  System.UITypes;

{ TFMXServicioDialogo }

function TVCLServicioDialogo.MessageDlg(const ATitulo, ATexto: String): Boolean;
begin
  Result := VCL.Dialogs.MessageDlg(ATitulo, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes
end;

procedure InitializePlatform;
begin
  ;
end;

initialization
  MVVMCore.RegistrarServicioDialogo(TVCLServicioDialogo);

end.
