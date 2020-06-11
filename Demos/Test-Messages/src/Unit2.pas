unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  MVVM.Messages.Engine, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TTestMessageInteger = class(TMessage)
  public
    Valor: Integer;

    constructor Create(const AValue: Integer); overload;
  end;

  TTestMessageString = class(TMessage)
  public
    Valor: String;

    constructor Create(const AValue: String); overload;
  end;

  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FValor            : Integer;
    FListenerInteger  : IMessageListener<TTestMessageInteger>;
    FListenerString1  : IMessageListener<TTestMessageString>;
    FListenerString2  : IMessageListener<TTestMessageString>;
  protected
    procedure OnTestMessageInteger(AMsg: IMessage);
    procedure OnTestMessageString1(AMsg: IMessage);
    procedure OnTestMessageString2(AMsg: IMessage);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  MVVM.Core,
  MVVM.Services.Platform.FMX;

{$R *.fmx}

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
var
  LMsg: IMessage;
begin
  LMsg := TTestMessageInteger.Create(FValor);
  LMsg.Queue;
  Inc(FValor);
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  LMsg: IMessage;
begin
  LMsg := TTestMessageString.Create('Este es un mensaje de prueba de tipo <string>');
  LMsg.Queue;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  MVVMCore.InitializationDone;
  InitializePlatform;
  FListenerInteger := TMessageListener<TTestMessageInteger>.Create;
  FListenerInteger.IsCodeToExecuteInUIMainThread := True;
  FListenerInteger.OnMessage.Add(OnTestMessageInteger);

  FListenerString1 := TMessageListener<TTestMessageString>.Create;
  FListenerString1.IsCodeToExecuteInUIMainThread := True;
  FListenerString1.OnMessage.Add(OnTestMessageString1);

  FListenerString2 := TMessageListener<TTestMessageString>.Create;
  FListenerString2.IsCodeToExecuteInUIMainThread := True;
  FListenerString2.OnMessage.Add(OnTestMessageString2);

  FValor := 1;
end;

procedure TForm2.OnTestMessageInteger(AMsg: IMessage);
begin
  Memo1.Lines.Add('Integer: ' + TTestMessageInteger(AMsg).Valor.ToString)
end;

procedure TForm2.OnTestMessageString1(AMsg: IMessage);
begin
  Memo1.Lines.Add('String (L1): ' + TTestMessageString(AMsg).Valor)
end;

procedure TForm2.OnTestMessageString2(AMsg: IMessage);
begin
  Memo1.Lines.Add('String (L2): ' + TTestMessageString(AMsg).Valor)
end;

{ TTestMessage }

constructor TTestMessageInteger.Create(const AValue: Integer);
begin
  inherited Create;
  Valor := AValue;
end;

{ TTestMessageString }

constructor TTestMessageString.Create(const AValue: String);
begin
  inherited Create;
  Valor := AValue;
end;

end.
