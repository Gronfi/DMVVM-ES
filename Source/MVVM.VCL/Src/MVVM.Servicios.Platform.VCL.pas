unit MVVM.Servicios.Platform.VCL;

interface

uses
  MVVM.Interfaces;

type
  TVCLPlatformServices = class(TPlatformServicesBase)
  public
    function MessageDlg(const ATitulo: string; const ATexto: String): Boolean; override;
    function IsMainThreadUI: Boolean; override;
  end;

  procedure InitializePlatform;

implementation

uses
  VCL.Dialogs,
  System.Classes,
  System.UITypes,

  MVVM.Core;

{ TFMXServicioDialogo }

function TVCLPlatformServices.IsMainThreadUI: Boolean;
begin
  Result := TThread.Current.ThreadID = MainThreadID;
end;

function TVCLPlatformServices.MessageDlg(const ATitulo, ATexto: String): Boolean;
begin
  Result := VCL.Dialogs.MessageDlg(ATitulo, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes
end;

procedure InitializePlatform;
begin
  ;
end;

initialization
  MVVMCore.RegisterPlatformServices(TVCLPlatformServices);

end.
