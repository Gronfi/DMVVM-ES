unit MVVM.Servicios.Platform.FMX;

interface

uses
  MVVM.Core;

type
  TFMXServicioDialogo = class(TServicioDialogoBase)
  public
    function MessageDlg(const ATitulo: string; const ATexto: String): Boolean; override;
  end;

implementation

uses
  FMX.Dialogs,
  System.UITypes;

{ TFMXServicioDialogo }

function TFMXServicioDialogo.MessageDlg(const ATitulo, ATexto: String): Boolean;
begin
  Result := FMX.Dialogs.MessageDlg(ATitulo, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes
end;

initialization
  MVVMCore.RegistrarServicioDialogo(TFMXServicioDialogo);

end.
