unit CSV.View;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Edit,
  FMX.Controls.Presentation,

  MVVM.Interfaces,
  CSV.Interfaces, FMX.Effects, FMX.Layouts, FMX.Objects,

  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX;

type
  TfrmCSV = class(TFormView<ICSVFile_ViewModel>, ICSVFile_View)
    Label1: TLabel;
    lePath: TEdit;
    btPath: TButton;
    ActionList1: TActionList;
    FileOpenDialog1: TOpenDialog;
    mmInfo: TMemo;
    btTNP: TButton;
    btTP: TButton;
    btNuevaVista: TButton;
    DoCrearVista: TAction;
    DoSelectFile: TAction;
    ShadowEffect1: TShadowEffect;
    rrProgreso: TRoundRect;
    ShadowEffect2: TShadowEffect;
    Label2: TLabel;
    pgBarra: TProgressBar;
    DoParalelo: TAction;
    DoNoParalelo: TAction;
    procedure btPathClick(Sender: TObject);
    procedure btTNPClick(Sender: TObject);
    procedure btTPClick(Sender: TObject);
    procedure DoCrearVistaExecute(Sender: TObject);
    procedure DoSelectFileExecute(Sender: TObject);
    procedure lePathChangeTracking(Sender: TObject);
  private
    { Private declarations }
    FViewModel: ICSVFile_ViewModel;

  protected
    procedure OnProcesoFinalizado(const ADato: String);
    procedure OnProgresoProceso(const ADato: Integer);

    procedure DoSeleccionarFichero;
    procedure DoCrearNuevaVista;
    procedure DoMetodoNoParalelo(Sender: TObject);
    procedure DoMetodoParalelo(Sender: TObject);
  public
    { Public declarations }
    procedure AddViewModel(AViewModel: IViewModel<ICSVFile_Model>);
    procedure RemoveViewModel(AViewModel: IViewModel<ICSVFile_Model>);
  end;

var
  frmCSV: TfrmCSV;

implementation

uses
  Spring;

{$R *.fmx}

{ TForm1 }

procedure TfrmCSV.AddViewModel(AViewModel: IViewModel<ICSVFile_Model>);
begin
  if FViewModel <> AViewModel then
  begin
    if Supports(AViewModel, ICSVFile_ViewModel, FViewModel)  then
    begin
      FViewModel := AViewModel as ICSVFile_ViewModel;
      //Bindings a capela

      //DoCrearVista.Bind(FViewModel.);
      DoParalelo.Bind(FViewModel.ProcesarFicheroCSV_Parallel, FViewModel.GetIsValidFile);
      DoNoParalelo.Bind(FViewModel.ProcesarFicheroCSV, FViewModel.GetIsValidFile);
      //Binder.
      (*
      //1) boton ejecución no paralela
      FViewModel.Bind('src', '"Test-No Parallel, File: " + src.FileName', btTNP, 'dst', 'dst.Text'); //formateo del caption
      FViewModel.Bind('IsValidFile', btTNP, 'Enabled'); //boton habilitado o no
      //2) boton ejecución en paralelo
      FViewModel.Bind('src', '"Test-Parallel, File: " + src.FileName', btTP, 'dst', 'dst.Text'); //formateo del caption
      FViewModel.Bind('IsValidFile', btTP, 'Enabled'); //boton habilitado o no
      //3) evento de fin de procesamiento
      FViewModel.OnProcesamientoFinalizado.Add(OnProcesoFinalizado);
      //4) evento de progreso
      FViewModel.OnProgresoProcesamiento.Add(OnProgresoProceso);
      *)
    end
    else raise Exception.Create('No casan las interfaces');
  end;
end;

procedure TfrmCSV.btPathClick(Sender: TObject);
begin
  DoSeleccionarFichero
end;

procedure TfrmCSV.btTNPClick(Sender: TObject);
begin
  DoMetodoNoParalelo;
end;

procedure TfrmCSV.btTPClick(Sender: TObject);
begin
  DoMetodoParalelo;
end;

procedure TfrmCSV.DoCrearNuevaVista;
var
  LVista: TfrmCSV;
begin
  Guard.CheckNotNull(FViewModel, 'ViewModel no asignado');
  LVista := TfrmCSV.Create(Self);
  LVista.AddViewModel(FViewModel);
  LVista.Show;
end;

procedure TfrmCSV.DoCrearVistaExecute(Sender: TObject);
begin
  DoCrearNuevaVista
end;

procedure TfrmCSV.DoMetodoNoParalelo(Sender: TObject);
begin
  Guard.CheckNotNull(FViewModel, 'ViewModel no asignado');
  if not FViewModel.ProcesarFicheroCSV then
    mmInfo.Lines.Add('Hay problemas')
  else mmInfo.Lines.Add('Ok')
end;

procedure TfrmCSV.DoMetodoParalelo(Sender: TObject);
begin
  Guard.CheckNotNull(FViewModel, 'ViewModel no asignado');
  if not FViewModel.ProcesarFicheroCSV_Parallel then
    mmInfo.Lines.Add('Hay problemas')
  else mmInfo.Lines.Add('Ok')
end;

procedure TfrmCSV.DoSeleccionarFichero;
begin
 if not String(lePath.Text).IsEmpty then
  begin
    FileOpenDialog1.FileName := lePath.Text;
  end;
  if FileOpenDialog1.Execute then
  begin
    lePath.Text := FileOpenDialog1.FileName;
  end;
end;

procedure TfrmCSV.DoSelectFileExecute(Sender: TObject);
begin
  DoSeleccionarFichero
end;

procedure TfrmCSV.lePathChangeTracking(Sender: TObject);
begin
  Guard.CheckNotNull(FViewModel, 'ViewModel no asignado');
  FViewModel.FileName := lePath.Text;
end;

procedure TfrmCSV.OnProcesoFinalizado(const ADato: String);
begin
  mmInfo.Lines.Add('Evento fin procesamiento: ' + ADato);
  rrProgreso.Visible := False;
end;

procedure TfrmCSV.OnProgresoProceso(const ADato: Integer);
begin
  if not rrProgreso.IsVisible then
    rrProgreso.Visible := True;
  pgBarra.Value        := ADato;
  Application.ProcessMessages;
end;

procedure TfrmCSV.RemoveViewModel(AViewModel: IViewModel<ICSVFile_Model>);
begin
  FViewModel := nil;
end;

end.
