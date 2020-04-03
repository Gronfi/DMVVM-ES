unit uDemoViewFrameTipo1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  uInterfaces,
  uAtributos,
  uDemoInterfaces,
  uDemoViewModels,
  uTypes,
  uView;

type
  [ViewForVM(IMyViewModel1, TMyViewModel1)]
  TFrame1 = class(TFrameView<IMyViewModel1, TMyViewModel1>)
    Memo1: TMemo;
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetVM_AsInterface: IMyViewModel1; override;
    function GetVM_AsObject: TMyViewModel1; override;
  end;

implementation

{$R *.fmx}

{ TFrame1 }

procedure TFrame1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add(GetVM_AsInterface.Hello1);
end;

function TFrame1.GetVM_AsInterface: IMyViewModel1;
begin
  Result := FViewModel as IMyViewModel1
end;

function TFrame1.GetVM_AsObject: TMyViewModel1;
begin
  Result := FViewModel as TMyViewModel1;
end;

end.
