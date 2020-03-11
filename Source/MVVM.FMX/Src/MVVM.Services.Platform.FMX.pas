unit MVVM.Services.Platform.FMX;

interface

uses
  MVVM.Interfaces;

type
  TFMXPlatformServices = class(TPlatformServicesBase)
  public
    function MessageDlg(const ATitulo: string; const ATexto: String): Boolean; override;
    function IsMainThreadUI: Boolean; override;
  end;

  procedure InitializePlatform;

implementation

uses
  FMX.Forms,
  FMX.Dialogs,
  System.Classes,
  System.UITypes,

  MVVM.Core;

{ TFMXServicioDialogo }

function TFMXPlatformServices.IsMainThreadUI: Boolean;
begin
  Result := TThread.Current.ThreadID = MainThreadID;
end;

function TFMXPlatformServices.MessageDlg(const ATitulo, ATexto: String): Boolean;
begin
  Result := FMX.Dialogs.MessageDlg(ATitulo, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes
end;

procedure InitializePlatform;
begin
  ;
end;

initialization
  MVVMCore.RegisterPlatformServices(TFMXPlatformServices);

end.
