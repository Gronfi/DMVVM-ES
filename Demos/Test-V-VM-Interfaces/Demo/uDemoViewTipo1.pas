unit uDemoViewTipo1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  System.Generics.Collections, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls,

  uInterfaces,
  uAtributos,
  uDemoInterfaces,
  uDemoViewModels,

  uView;

type
  [ViewForVM(IMyViewModel1, TMyViewModel1)]
  TfrmIntf1 = class(TFormView<IMyViewModel1, TMyViewModel1>)
    Memo1: TMemo;
    Button1: TButton;
    Button3: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  protected
    function GetVM_AsInterface: IMyViewModel1; override;
    function GetVM_AsObject: TMyViewModel1; override;
  public
    { Public declarations }
  end;

(*
  [ViewForVM(IMyViewModel1, TMyViewModel1)]
  TForm68 = class(TForm, IVM_User<IMyViewModel1, TMyViewModel1>)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FViewModel : IViewModel;

  protected
    function GetVM_AsInterface: IMyViewModel1;
    function GetVM_AsObject: TMyViewModel1;

    procedure CreateVM;
  public
    { Public declarations }
    property VM_AsInterface: IMyViewModel1 read GetVM_AsInterface;
    property VM_AsObject: TMyViewModel1 read GetVM_AsObject;
  end;
*)

var
  frmIntf1: TfrmIntf1;

implementation

uses
  System.RTTI,

  uDemoViewTipo2,
  uDemoViewTipo3;

{$R *.fmx}

procedure TfrmIntf1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add(GetVM_AsInterface.Hello1);
end;

function TfrmIntf1.GetVM_AsObject: TMyViewModel1;
begin
  Result := FViewModel as TMyViewModel1;
end;

procedure TfrmIntf1.Button2Click(Sender: TObject);
var
  LForm: TfrmIntf2;
begin
  LForm := TfrmIntf2.Create(Self);
  LForm.Show;
end;

procedure TfrmIntf1.Button3Click(Sender: TObject);
var
  LForm: TfrmMultiVista;
begin
  LForm := TfrmMultiVista.Create(Self);
  LForm.Show;
end;

function TfrmIntf1.GetVM_AsInterface: IMyViewModel1;
begin
  Result := FViewModel as IMyViewModel1
end;

end.
