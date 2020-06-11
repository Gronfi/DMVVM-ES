unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  MVVM.Messages.Engine, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TThreadPublisher = class(TThread)
  protected
    procedure Execute; override;
  end;

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

  TTestMessageGeneric_Integer = class(TMessage_Generic<Integer>);


  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Memo2: TMemo;
    ToolBar1: TToolBar;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
  private
    { Private declarations }
    FValor            : Integer;
    FListenerInteger  : IMessageListener<TTestMessageInteger>;
    FListenerString1  : IMessageListener<TTestMessageString>;
    FListenerString2  : IMessageListener<TTestMessageString>;

    FListenerGeneric  : IMessageListener<TTestMessageGeneric_Integer>;

    FListenerTest : IMessageListener<TTestMessageInteger>;
  protected
    procedure DisableChecks;

    procedure OnTestMessageInteger(AMsg: IMessage);
    procedure OnTestMessageString1(AMsg: IMessage);
    procedure OnTestMessageString2(AMsg: IMessage);
    procedure OnTestMessageGeneric_Integer(AMsg: IMessage);
    procedure OnTestMessageIntegerMemo2(AMsg: IMessage);
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

procedure TForm2.Button3Click(Sender: TObject);
var
  LMsg: IMessage;
begin
  LMsg := TTestMessageGeneric_Integer.Create;
  TTestMessageGeneric_Integer(LMsg).Data := FValor;
  LMsg.Queue;
  Inc(FValor);
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  LPool: TMessageChannel_Main_SingleThreaded;
begin
  DisableChecks;
  LPool := MVVMCore.IoC.Resolve<TMessageChannel_Main_SingleThreaded>;
  FListenerTest := TMessageListener<TTestMessageInteger>.Create(LPool);
  FListenerTest.IsCodeToExecuteInUIMainThread := True;
  FListenerTest.OnMessage.Add(OnTestMessageIntegerMemo2);
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  LPool: TMessageChannel_Main;
begin
  DisableChecks;
  LPool := MVVMCore.IoC.Resolve<TMessageChannel_Main>;
  FListenerTest := TMessageListener<TTestMessageInteger>.Create(LPool);
  FListenerTest.IsCodeToExecuteInUIMainThread := True;
  FListenerTest.OnMessage.Add(OnTestMessageIntegerMemo2);
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  LThread: TThreadPublisher;
begin
  LThread := TThreadPublisher.Create(True);
  LThread.FreeOnTerminate := True;
  LThread.Start;
end;

procedure TForm2.CheckBox1Change(Sender: TObject);
begin
  FListenerInteger.Enabled := CheckBox1.IsChecked
end;

procedure TForm2.CheckBox2Change(Sender: TObject);
begin
  FListenerString1.Enabled := CheckBox2.IsChecked;
end;

procedure TForm2.CheckBox3Change(Sender: TObject);
begin
  FListenerGeneric.Enabled := CheckBox3.IsChecked
end;

procedure TForm2.CheckBox4Change(Sender: TObject);
begin
  FListenerString2.Enabled := CheckBox4.IsChecked
end;

procedure TForm2.DisableChecks;
begin
  CheckBox1.IsChecked := False;
  CheckBox2.IsChecked := False;
  CheckBox3.IsChecked := False;
  CheckBox4.IsChecked := False;
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

  FListenerGeneric  := TMessageListener<TTestMessageGeneric_Integer>.Create;
  FListenerGeneric.IsCodeToExecuteInUIMainThread := True;
  FListenerGeneric.OnMessage.Add(OnTestMessageGeneric_Integer);

  FValor := 1;
end;

procedure TForm2.OnTestMessageGeneric_Integer(AMsg: IMessage);
begin
  Memo1.Lines.Add('Generic-Integer: ' + TMessage_Generic<Integer>(AMsg).Data.ToString)
end;

procedure TForm2.OnTestMessageInteger(AMsg: IMessage);
begin
  Memo1.Lines.Add('Integer: ' + TTestMessageInteger(AMsg).Valor.ToString)
end;

procedure TForm2.OnTestMessageIntegerMemo2(AMsg: IMessage);
begin
  Memo2.Lines.Add(TTestMessageInteger(AMsg).Valor.ToString)
end;

procedure TForm2.OnTestMessageString1(AMsg: IMessage);
begin
  Memo1.Lines.Add('String (Subscriber1): ' + TTestMessageString(AMsg).Valor)
end;

procedure TForm2.OnTestMessageString2(AMsg: IMessage);
begin
  Memo1.Lines.Add('String (Subscriber2): ' + TTestMessageString(AMsg).Valor)
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

{ TThreadPublisher }

procedure TThreadPublisher.Execute;
var
  I   : Integer;
  LMsg: IMessage;
begin
  for I := 1 to 1000 do
  begin
    LMsg := TTestMessageInteger.Create(I);
    LMsg.Queue;
  end;
end;

end.
