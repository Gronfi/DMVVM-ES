unit Unit68;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Grid, FMX.Controls.Presentation, FMX.ScrollBox,

  System.Generics.Collections, FMX.StdCtrls;

type
  TTest = class
    public
      Nombre: string;
      Apellido: string;
  end;

  TForm68 = class(TForm)
    Grid1: TGrid;
    Column1: TColumn;
    Column2: TColumn;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Grid1GetValue(Sender: TObject; const ACol, ARow: Integer;
      var Value: TValue);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FList: TList<TTest>;
  public
    { Public declarations }
  end;

var
  Form68: TForm68;

implementation

{$R *.fmx}

procedure TForm68.Button1Click(Sender: TObject);
var
  LTest: TTest;
begin
  LTest := TTest.Create;
  LTest.Nombre   := 'Nombre_' + DateTimeToStr(Now);
  LTest.Apellido := 'Apellido_' + DateTimeToStr(Now);
  FList.Add(LTest);
  Grid1.RowCount := FList.Count;
end;

procedure TForm68.FormCreate(Sender: TObject);
var
  LTest: TTest;
begin
  FList := TList<TTest>.Create;
  LTest := TTest.Create;
  LTest.Nombre   := 'David';
  LTest.Apellido := 'Alcelay';
  FList.Add(LTest);
  LTest := TTest.Create;
  LTest.Nombre   := 'Teo';
  LTest.Apellido := 'Carras';
  FList.Add(LTest);
  LTest := TTest.Create;
  LTest.Nombre   := 'Joseba';
  LTest.Apellido := 'Garci';
  FList.Add(LTest);
  Grid1.RowCount := FList.Count;
end;

procedure TForm68.Grid1GetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
begin
  if ARow >= FList.Count then
    begin
      Value := '?';
      Exit;
    end;
    case ACol of
      0:
        begin
          Value := FList[ARow].Nombre;
        end;
      1:
        begin
          Value := FList[ARow].Apellido;
        end;
    end;
end;

end.
