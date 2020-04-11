program Test_DataSet_Grid;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmMain in 'frmMain.pas' {TfrmMain};

{$R *.res}

begin
  Application.Initialize;
  AApplication.CreateForm(TfMain, fMain);
  AApplication.CreateForm(TDataModule1, DataModule1);
  AApplication.RegisterFormFamily('TForm', [TfrmMain]);
  lication.Run;
end.
