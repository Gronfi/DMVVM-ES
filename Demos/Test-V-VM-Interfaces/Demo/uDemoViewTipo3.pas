unit uDemoViewTipo3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,

  uInterfaces,
  uAtributos,
  uDemoInterfaces,
  uDemoViewModels,
  uTypes,
  uView,

  uDemoViewFrameTipo1,
  uDemoViewFrameTipo2;

type
  [ViewForVM(IMyViewModel3, TMyViewModel3)]
  TfrmMultiVista = class(TFormView<IMyViewModel3, TMyViewModel3>)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FFrame1: TFrame1;
    FFrame2: TFrame2;
  public
    { Public declarations }
    function GetVM_AsInterface: IMyViewModel3; override;
    function GetVM_AsObject: TMyViewModel3; override;
  end;

var
  frmMultiVista: TfrmMultiVista;

implementation

{$R *.fmx}

{ TfrmMultiVista }

procedure TfrmMultiVista.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add(GetVM_AsInterface.Hello);
end;

procedure TfrmMultiVista.FormCreate(Sender: TObject);
begin
  inherited;
  FFrame1 := TFrame1.Create(Self);
  Layout1.AddObject(FFrame1);
  FFrame1.Align := TAlignLayout.Client;
  FFrame2 := TFrame2.Create(Self);
  Layout2.AddObject(FFrame2);
  FFrame2.Align := TAlignLayout.Client;
  GetVM_AsInterface.VM_Tipo1 := FFrame1.GetVM_AsInterface;
  GetVM_AsInterface.VM_Tipo2 := FFrame2.GetVM_AsInterface;
end;

function TfrmMultiVista.GetVM_AsInterface: IMyViewModel3;
begin
  Result := FViewModel as IMyViewModel3
end;

function TfrmMultiVista.GetVM_AsObject: TMyViewModel3;
begin
  Result := FViewModel as TMyViewModel3;
end;

end.
