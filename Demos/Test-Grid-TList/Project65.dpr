program Project65;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit68 in 'Unit68.pas' {Form68};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm68, Form68);
  Application.Run;
end.
