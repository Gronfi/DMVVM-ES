program Test_DataSet_Grid;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmMain in 'frmMain.pas' {TfrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, fMain);
  Application.RegisterFormFamily('TForm', [TfrmMain]);
  Application.Run;
end.
