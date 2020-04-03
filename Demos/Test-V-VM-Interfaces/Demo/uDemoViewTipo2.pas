unit uDemoViewTipo2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,

  uInterfaces,
  uAtributos,
  uDemoInterfaces,
  uDemoViewModels,
  uTypes,
  uView;

type
  [ViewForVM(IMyViewModel2, TMyViewModel2, EInstanceType.itSingleton)]
  TfrmIntf2 = class(TFormView<IMyViewModel2, TMyViewModel2>)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  protected
  public
    { Public declarations }
    function GetVM_AsInterface: IMyViewModel2; override;
    function GetVM_AsObject: TMyViewModel2; override;
  end;

var
  frmIntf2: TfrmIntf2;

implementation

{$R *.fmx}

{ TfrmIntf2 }

procedure TfrmIntf2.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add(GetVM_AsInterface.Hello2);
end;

function TfrmIntf2.GetVM_AsInterface: IMyViewModel2;
begin
  Result := FViewModel as IMyViewModel2
end;

function TfrmIntf2.GetVM_AsObject: TMyViewModel2;
begin
  Result := FViewModel as TMyViewModel2;
end;

end.
