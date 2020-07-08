unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Objects,

  Spring, Spring.Collections,

  MVVM.Messages.Engine,
  MVVM.Messages.Engine.Interfaces,
  MVVM.Types;

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

  TTestMessageInteger_Filter = class(TMessage)
  public
    Valor : Integer;
    Filter: String;

    constructor Create(const AValue: Integer; const AFilter: String); overload;
  end;

  TTestMessageInteger_Filter_Listener = class(TMessageListener<TTestMessageInteger_Filter>)
  private
    FFilter: String;
  protected
    function GetConditionsMatch(AMessage: IMessage): Boolean; override;
  public
    constructor Create(const AFilter: string; const AChannel: String = ''; const AFilterCondition: TListenerFilter = nil; const ACodeExecutesInMainUIThread: Boolean = False;
                       const ATypeRestriction: EMessageTypeRestriction = EMessageTypeRestriction.mtrAllowDescendants); overload;

    property Filter: string read FFilter write FFilter;
  end;

  ITest = interface
    ['{621B7280-C06D-4123-B8DC-7BF02EA898E4}']
  end;

  TTest = class(TInterfacedObject, ITest)
  public
    constructor Create;
    destructor Destroy; override;
  end;

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
    ToolBar2: TToolBar;
    Rectangle1: TRectangle;
    cbPooled: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo3: TMemo;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
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
    procedure cbPooledChange(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    { Private declarations }
    FValor            : Integer;
    FListenerInteger  : IMessageListener<TTestMessageInteger>;
    FListenerInteger2 : IMessageListener<TTestMessageInteger>;
    FListenerString1  : IMessageListener<TTestMessageString>;
    FListenerString2  : IMessageListener<TTestMessageString>;

    FListenerGeneric  : IMessageListener<TTestMessageGeneric_Integer>;

    FListenerTest : IMessageListener<TTestMessageInteger>;

    FListenerFilter1: IMessageListener<TTestMessageInteger_Filter>;
    FListenerFilter2: IMessageListener<TTestMessageInteger_Filter>;

    function LogTime: string;
  protected
    procedure DisableChecks;

    procedure OnTestMessageInteger(AMsg: IMessage);
    procedure OnTestMessageInteger2(AMsg: IMessage);
    procedure OnTestMessageString1(AMsg: IMessage);
    procedure OnTestMessageString2(AMsg: IMessage);
    procedure OnTestMessageGeneric_Integer(AMsg: IMessage);
    procedure OnTestMessageIntegerMemo2(AMsg: IMessage);

    procedure OnFilteredMessage1(AMsg: IMessage);
    procedure OnFilteredMessage2(AMsg: IMessage);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  System.DateUtils,

  MVVM.Core,
  MVVM.Services.Platform.FMX;

{$R *.fmx}

{ TForm2 }

procedure TForm2.Button10Click(Sender: TObject);
var
  LHour, LMinute, LSecond, LMilliSecond: Word;
  LMsg                                 : IMessage;
  LNewDateTime                         : TDateTime;
begin
  DecodeTime(Now, LHour, LMinute, LSecond, LMilliSecond);
  LNewDateTime := RecodeTime(Now, LHour, LMinute, LSecond + 3, LMilliSecond);
  Memo1.Lines.Add(LogTime + 'Scheduled');
  LMsg := TTestMessageInteger.Create(8888);
  LMsg.Schedule(LNewDateTime);
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  LMsg: IMessage;
begin
  LMsg := TTestMessageInteger.Create(FValor);
  LMsg.Post;
  Inc(FValor);
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  LMsg: IMessage;
begin
  LMsg := TTestMessageString.Create('Este es un mensaje de prueba de tipo <string>');
  LMsg.Post;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  LMsg: IMessage;
begin
  LMsg := TTestMessageGeneric_Integer.Create;
  TTestMessageGeneric_Integer(LMsg).Data := FValor;
  LMsg.Post;
  Inc(FValor);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  DisableChecks;
  if Assigned(FListenerTest) then
  begin
    FListenerTest.Unregister;
    FListenerTest := nil;
  end;
  FListenerTest := TMessageListener<TTestMessageInteger>.Create(DEFAULT_CHANNEL_SINGLED_THREADED);
  FListenerTest.IsCodeToExecuteInUIMainThread := True;
  FListenerTest.OnMessage.Add(OnTestMessageIntegerMemo2);
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  DisableChecks;
  if Assigned(FListenerTest) then
  begin
    FListenerTest.Unregister;
    FListenerTest := nil;
  end;
  FListenerTest := TMessageListener<TTestMessageInteger>.Create(DEFAULT_CHANNEL_MULTI_THREADED);
  FListenerTest.IsCodeToExecuteInUIMainThread := True;
  FListenerTest.OnMessage.Add(OnTestMessageIntegerMemo2);
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  LThread: TThreadPublisher;
begin
  Memo2.Lines.Clear;
  LThread := TThreadPublisher.Create(True);
  LThread.FreeOnTerminate := True;
  LThread.Start;
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  FListenerFilter1 := TTestMessageInteger_Filter_Listener.Create('Filter1');
  FListenerFilter1.IsCodeToExecuteInUIMainThread := True;
  FListenerFilter1.OnMessage.Add(OnFilteredMessage1);
  FListenerFilter2 := TTestMessageInteger_Filter_Listener.Create('Filter2');
  FListenerFilter2.IsCodeToExecuteInUIMainThread := True;
  FListenerFilter2.OnMessage.Add(OnFilteredMessage2);
end;

procedure TForm2.Button8Click(Sender: TObject);
var
  LMsg: IMessage;
  I   : Integer;
begin
  for I := 1 to 10 do
  begin
    case I Mod 2 of
      0:
        begin
          LMsg := TTestMessageInteger_Filter.Create(I, 'Filter2');
          LMsg.Post;
        end
      else begin
             LMsg := TTestMessageInteger_Filter.Create(I, 'Filter1');
             LMsg.Post;
           end;
    end;
  end;
end;

procedure TForm2.Button9Click(Sender: TObject);
var
  LMsg: IMessage;
begin
  Memo1.Lines.Add(LogTime + 'Scheduled');
  LMsg := TTestMessageInteger.Create(9999);
  LMsg.Schedule(2000);
end;

procedure TForm2.cbPooledChange(Sender: TObject);
begin
  case cbPooled.IsChecked of
    True:
      begin
        MessageBus.MessageDeploymentKind := EMessageDeploymentKind.mdkPooled
      end;
    False:
      begin
        MessageBus.MessageDeploymentKind := EMessageDeploymentKind.mdkFifo
      end;
  end;
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

  FListenerInteger2 := TMessageListener<TTestMessageInteger>.Create;
  FListenerInteger2.IsCodeToExecuteInUIMainThread := False;
  FListenerInteger2.OnMessage.Add(OnTestMessageInteger2);

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

  cbPooled.isChecked := (MessageBus.MessageDeploymentKind = EMessageDeploymentKind.mdkPooled);
end;

function TForm2.LogTime: string;
begin
  Result := FormatDateTime('hhnnss.zzz ', Now);
end;

procedure TForm2.OnFilteredMessage1(AMsg: IMessage);
begin
  Memo3.Lines.Add(LogTime + 'Filtered (Listener1): ' + TTestMessageInteger_Filter(AMsg).Valor.ToString)
end;

procedure TForm2.OnFilteredMessage2(AMsg: IMessage);
begin
  Memo3.Lines.Add(LogTime + 'Filtered (Listener2): ' + TTestMessageInteger_Filter(AMsg).Valor.ToString)
end;

procedure TForm2.OnTestMessageGeneric_Integer(AMsg: IMessage);
begin
  Memo1.Lines.Add(LogTime + 'Generic-Integer: ' + TMessage_Generic<Integer>(AMsg).Data.ToString)
end;

procedure TForm2.OnTestMessageInteger(AMsg: IMessage);
begin
  Memo1.Lines.Add(LogTime + 'Integer: ' + TTestMessageInteger(AMsg).Valor.ToString)
end;

procedure TForm2.OnTestMessageInteger2(AMsg: IMessage);
begin
  Sleep(10000);
  TThread.Queue(nil,
    procedure
    begin
      Memo1.Lines.Add(LogTime + ' Fin!');
    end);
end;

procedure TForm2.OnTestMessageIntegerMemo2(AMsg: IMessage);
begin
  Memo2.Lines.Add(TTestMessageInteger(AMsg).Valor.ToString)
end;

procedure TForm2.OnTestMessageString1(AMsg: IMessage);
begin
  Memo1.Lines.Add(LogTime + 'String (Subscriber1): ' + TTestMessageString(AMsg).Valor)
end;

procedure TForm2.OnTestMessageString2(AMsg: IMessage);
begin
  Memo1.Lines.Add(LogTime + 'String (Subscriber2): ' + TTestMessageString(AMsg).Valor)
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
    LMsg.Post;
  end;
end;

{ TTestMessageInteger_Filter }

constructor TTestMessageInteger_Filter.Create(const AValue: Integer; const AFilter: String);
begin
  inherited Create;
  Valor := AValue;
  Filter:= AFilter;
end;

{ TTestMessageInteger_Filter_Listener }

constructor TTestMessageInteger_Filter_Listener.Create(const AFilter: string; const AChannel: String; const AFilterCondition: TListenerFilter; const ACodeExecutesInMainUIThread: Boolean; const ATypeRestriction: EMessageTypeRestriction);
begin
  FFilter := AFilter;
  Create(AChannel, AFilterCondition, ACodeExecutesInMainUIThread, ATypeRestriction);
end;

function TTestMessageInteger_Filter_Listener.GetConditionsMatch(AMessage: IMessage): Boolean;
begin
  Result := FFilter.Equals(TTestMessageInteger_Filter(AMessage).Filter)
end;

{ TTest }

constructor TTest.Create;
begin
  inherited;
end;

destructor TTest.Destroy;
begin

  inherited;
end;

end.
