unit Coche.Types;

interface

type

  RCoche = record
    ID: Integer;
    Nombre: string;
    Imagen: TObject;
    Dueño: Integer;
  end;

  TNotify_Coche = procedure(const AData: RCoche) of object;

implementation

end.
