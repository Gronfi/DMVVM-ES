unit CSV.View;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Edit,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Objects, FMX.Effects,

  CSV.Interfaces,

  MVVM.Attributes,
  MVVM.Interfaces.Architectural,
  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX;

type
  [View_For_ViewModel(ICSVFile_View_NAME, ICSVFile_ViewModel, 'WINDOWS_DESKTOP')]
  [View_For_ViewModel(ICSVFile_View_NAME, ICSVFile_ViewModel, 'LINUX_DESKTOP')]
  TfrmCSV = class(TFormView<ICSVFile_ViewModel>)
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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DoSelectFileExecute(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure OnProcesoFinalizado(const ADato: String);
    procedure OnProgresoProceso(const ADato: Integer);

    procedure DoSeleccionarFichero;

    procedure SetupView; override;
  public
    { Public declarations }
  end;

var
  frmCSV: TfrmCSV;

implementation

uses
  MVVM.ViewFactory,
  MVVM.Types,
  MVVM.Core,

  CSV.ViewModel,

  Spring;

{$R *.fmx}

procedure TfrmCSV.Button1Click(Sender: TObject);
begin
  MVVMCore.DisableBinding(DoCrearVista);
end;

procedure TfrmCSV.Button2Click(Sender: TObject);
begin
  MVVMCore.EnableBinding(DoCrearVista);
end;

procedure TfrmCSV.Button3Click(Sender: TObject);
begin
  lePath.Free;
end;

procedure TfrmCSV.Button4Click(Sender: TObject);
begin
  lePath := TEdit.Create(Self);
  lePath.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];
  lePath.Position.X := -4;
  lePath.Position.Y := 16;
  lePath.Size.Width := 405;
  lePath.Size.Height := 22;
  lePath.TextPrompt := 'Introduce el path';
  label1.AddObject(lePath);

  //bindings
  Binder.Bind(TCSVFile_ViewModel(ViewModel), 'FileName', lePath, 'Text', EBindDirection.TwoWay, [EBindFlag.TargetTracking]);
end;

{ TForm1 }

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

procedure TfrmCSV.SetupView;
begin
  //Code Bindings
  //1) commands, actions
  DoParalelo.Bind(TCSVFile_ViewModel(ViewModel).ProcesarFicheroCSV_Parallel, TCSVFile_ViewModel(ViewModel).GetIsValidFile);
  Binder.BindAction(DoParalelo);

  DoNoParalelo.Bind(TCSVFile_ViewModel(ViewModel).ProcesarFicheroCSV, TCSVFile_ViewModel(ViewModel).GetIsValidFile);
  Binder.BindAction(DoNoParalelo);

  DoCrearVista.Bind(TCSVFile_ViewModel(ViewModel).CreateNewView);
  Binder.BindAction(DoCrearVista);

  //2) buttons captions
  Binder.Bind(ViewModel.GetAsObject, '"Test-No Parallel, File: " + Src.FileName', btTNP, 'Dst.Text', EBindDirection.OneWay, [EBindFlag.UsesExpressions]);
  Binder.Bind(ViewModel.GetAsObject, '"Test-Parallel, File: " + Src.FileName', btTP, 'Dst.Text', EBindDirection.OneWay, [EBindFlag.UsesExpressions]);

  //3) event end of processing file
  ViewModel.OnProcesamientoFinalizado.Add(OnProcesoFinalizado);

  //4) event progress of processing file
  ViewModel.OnProgresoProcesamiento.Add(OnProgresoProceso);

  //5) Update filename
  Binder.Bind(TCSVFile_ViewModel(ViewModel), 'FileName', lePath, 'Text', EBindDirection.TwoWay, [EBindFlag.TargetTracking]);
end;

initialization
  TfrmCSV.Classname; //as there should be no implicit create, we must do this so the rtti info of the class is included in the final exe

end.
