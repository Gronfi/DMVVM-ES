unit MVVM.Core;

interface

uses
  Spring;

type
  IServicioDialogo = interface
    ['{95F9A402-2D01-48E5-A38B-9A6202FF5F59}']
    function MessageDlg(const ATitulo: string; const ATexto: String): Boolean;
  end;

  TServicioDialogoBase = class abstract(TInterfacedObject, IServicioDialogo)
  public
    function MessageDlg(const ATitulo: string; const ATexto: String): Boolean; virtual; abstract;
  end;

  TServicioDialogoClass = class of TServicioDialogoBase;

  MVVMCore = record
  private
    class var FServicioDialogoClass: TServicioDialogoClass;
  public
    class procedure RegistrarServicioDialogo(AServicioClass: TServicioDialogoClass); static;
    class function ServicioDialogo: IServicioDialogo; static;
  end;

implementation

{ MVVM }

class procedure MVVMCore.RegistrarServicioDialogo(AServicioClass: TServicioDialogoClass);
begin
  FServicioDialogoClass := AServicioClass;
end;

class function MVVMCore.ServicioDialogo: IServicioDialogo;
begin
  Spring.Guard.CheckNotNull(FServicioDialogoClass, 'No hay registrado ningun servicio para el dialogo');
  Result := FServicioDialogoClass.Create;
end;

end.
