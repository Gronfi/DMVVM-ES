unit uDemoViewFrameTipo2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation,

  uInterfaces,
  uAtributos,
  uDemoInterfaces,
  uDemoViewModels,
  uTypes,
  uView, FMX.ScrollBox, FMX.Memo;

type
  [ViewForVM(IMyViewModel2, TMyViewModel2)]
  TFrame2 = class(TFrameView<IMyViewModel2, TMyViewModel2>)
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetVM_AsInterface: IMyViewModel2; override;
    function GetVM_AsObject: TMyViewModel2; override;
  end;

implementation

{$R *.fmx}

procedure TFrame2.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add(GetVM_AsInterface.Hello2);
end;

function TFrame2.GetVM_AsInterface: IMyViewModel2;
begin
  Result := FViewModel as IMyViewModel2
end;

function TFrame2.GetVM_AsObject: TMyViewModel2;
begin
  Result := FViewModel as TMyViewModel2;
end;

end.
