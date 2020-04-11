{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2020 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}
{.$DEFINE PUREPASCAL} // remove the dot to use the RTTI version on x86/x64

unit Spring.Events;

interface

uses
  Classes,
  Rtti,
  SysUtils,
  Spring,
  Spring.Events.Base,
  TypInfo;

type

  {$REGION 'TEvent'}

  TEvent = class(TEventBase{$IFNDEF PUREPASCAL}, TProc{$ENDIF})
  private
    fTypeInfo: PTypeInfo;
  {$IFDEF PUREPASCAL}
    fProxy: Pointer;
  protected
    procedure InternalInvokeMethod(UserData: Pointer;
      const Args: TArray<TValue>; out Result: TValue); virtual;
    procedure InternalInvokeDelegate(Method: TRttiMethod;
      const Args: TArray<TValue>; out Result: TValue); virtual;
  {$ELSE}
    fInvocations: TObject;
    procedure Invoke;
  protected
    procedure InternalInvoke(Params: Pointer; StackSize: Integer); virtual;
  {$ENDIF}
    procedure Notify(Sender: TObject; const Item: TMethodPointer;
      Action: TEventBase.TCollectionNotification); override;
  public
    constructor Create(typeInfo: PTypeInfo);
    destructor Destroy; override;
  end;

  {$ENDREGION}


  {$REGION 'TEvent<T>'}

  TEvent<T> = class(TEvent, IEvent<T>)
  private
    function GetInvoke: T;
  public
    constructor Create;

    procedure Add(handler: T);
    procedure Remove(handler: T);
  end;

  IMulticastNotifyEvent = IEvent<TNotifyEvent>;

  TMulticastNotifyEvent = TEvent<TNotifyEvent>;

  {$ENDREGION}


  {$REGION 'TNotifyEventImpl'}

  TNotifyEventImpl = class(TEventBase, INotifyEvent)
  private
    function GetInvoke: TNotifyEvent;
    procedure InternalInvoke(sender: TObject);
  public
    constructor Create;
    procedure Add(handler: TNotifyEvent);
    procedure Remove(handler: TNotifyEvent);
    property Invoke: TNotifyEvent read GetInvoke;
  end;

  {$ENDREGION}


  {$REGION 'TNotifyEventImpl<T>'}

  TNotifyEventImpl<T> = class(TEventBase, INotifyEvent<T>)
  private
    function GetInvoke: TNotifyEvent<T>;
    procedure Add(handler: TNotifyEvent<T>);
    procedure Remove(handler: TNotifyEvent<T>);
    procedure InternalInvoke(sender: TObject; const item: T);
  public
    constructor Create;
  end;

  {$ENDREGION}


  {$REGION 'TPropertyChangedEventImpl'}

  TPropertyChangedEventImpl = class(TEventBase, IPropertyChangedEvent)
  private
    function GetInvoke: TPropertyChangedEvent;
    procedure Add(handler: TPropertyChangedEvent);
    procedure Remove(handler: TPropertyChangedEvent);
    procedure InternalInvoke(Sender: TObject;
      const EventArgs: IPropertyChangedEventArgs);
  public
    constructor Create;
  end;

  {$ENDREGION}


procedure EnsureInitialized(var instance; typeInfo: PTypeInfo);

implementation

uses
{$IFDEF PUREPASCAL}
  Spring.VirtualInterface,
{$ENDIF}
  Spring.ResourceStrings;


type
  TMethodEvent = class(TEvent, IEvent<TMethodPointer>)
  private
    procedure Add(handler: TMethodPointer);
    procedure Remove(handler: TMethodPointer);
  end;

  TDelegateEvent = class(TEvent, IEvent<IInterface>)
  private
    function GetInvoke: IInterface;
    procedure Add(handler: IInterface);
    procedure Remove(handler: IInterface);
  end;

procedure EnsureInitialized(var instance; typeInfo: PTypeInfo);
begin
  if Pointer(instance) = nil then
    if typeInfo.Kind = tkMethod then
      IEvent<TMethodPointer>(instance) := TMethodEvent.Create(typeInfo)
    else
      IEvent<IInterface>(instance) := TDelegateEvent.Create(typeInfo);
end;


{$REGION 'Proxy generators'}

{$IFNDEF PUREPASCAL}
type
  TMethodInvocations = class
  private
    const
      paEAX = Word(0);
      paEDX = Word(1);
      paECX = Word(2);
      paStack = Word(3);

    type
      PParameterInfos = ^TParameterInfos;
      TParameterInfos = array[0..255] of ^PTypeInfo;

      PParameters = ^TParameters;
      TParameters = packed record
      public
{$IFNDEF CPUX64}
        Registers: array[paEDX..paECX] of Cardinal;
        EAXRegister: Cardinal;
        ReturnAddress: Pointer;
{$ENDIF}
        Stack: array[0..1023] of Byte;
      end;

      PMethodInfo = ^TMethodInfo;
      TMethodInfo = record
        ParamInfos: PParameterInfos;
        StackSize: Integer;
        CallConvention: TCallConv;
{$IFDEF CPUX64}
        RegisterFlag: Word;
{$ENDIF}
        constructor Create(typeData: PTypeData);
      end;

      TMethodInvokeEvent = procedure(Params: Pointer; StackSize: Integer) of object;
  private
    fMethodInfo: TMethodInfo;
    fMethodInvokeEvent: TMethodInvokeEvent;
  protected
    procedure InternalInvokeHandlers(Params: PParameters);
    procedure InvokeEventHandlerStub;
  public
    constructor Create(methodTypeData: PTypeData; methodInvokeEvent: TMethodInvokeEvent);
  end;

function AdditionalInfoOf(TypeData: PTypeData): Pointer;
var
  P: PByte;
  I: Integer;
begin
  P := @TypeData^.ParamList;
  // Skip parameter names and types
  for I := 1 to TypeData^.ParamCount do //FI:W528
  begin
    Inc(P, 1 + P[1] + 1);
    Inc(P, P[0] + 1 );
  end;
  if TypeData^.MethodKind = mkFunction then
    // Skip return type name and info
    Inc(P, P[0] + 1 + 4);
  Result := P;
end;

procedure GetMethodTypeData(Method: TRttiMethod; var TypeData: PTypeData);

  procedure WriteByte(var Dest: PByte; b: Byte);
  begin
    Dest[0] := b;
    Inc(Dest);
  end;

  procedure WritePackedShortString(var Dest: PByte; const s: string);
{$IFNDEF NEXTGEN}
  begin
    PShortString(Dest)^ := ShortString(s);
    Inc(Dest, Dest[0] + 1);
  end;
{$ELSE}
  var
    buffer: TBytes;
  begin
    buffer := TEncoding.ANSI.GetBytes(s);
    if (Length(buffer) > 255) then SetLength(buffer, 255);
    Dest^ := Length(buffer);
    Inc(Dest);
    Move(buffer[0], Dest^, Length(buffer));
    Inc(Dest, Length(buffer));
  end;
{$ENDIF}

  procedure WritePointer(var Dest: PByte; p: Pointer);
  begin
    PPointer(Dest)^ := p;
    Inc(Dest, SizeOf(Pointer));
  end;

var
  params: TArray<TRttiParameter>;
  i: Integer;
  p: PByte;
begin
  TypeData.MethodKind := Method.MethodKind;
  params := Method.GetParameters;
  TypeData.ParamCount := Length(params);
  p := @TypeData.ParamList;
  for i := Low(params) to High(params) do
  begin
    WriteByte(p, Byte(params[i].Flags));
    WritePackedShortString(p, params[i].Name);
    WritePackedShortString(p, params[i].ParamType.Name);
  end;
  if method.MethodKind = mkFunction then
  begin
    WritePackedShortString(p, method.ReturnType.Name);
    WritePointer(p, method.ReturnType.Handle);
  end;
  WriteByte(p, Byte(method.CallingConvention));
  for i := Low(params) to High(params) do
  begin
    WritePointer(p, Pointer(NativeInt(params[i].ParamType.Handle) - SizeOf(Pointer)));
  end;
end;

procedure InvokeMethod(const Method: TMethod;
  Parameters: Pointer; StackSize: Integer);
const
  PointerSize = SizeOf(Pointer);
  paEDX = Word(1);
  paECX = Word(2);
type
  PParameters = ^TParameters;
  TParameters = packed record
  public
{$IFNDEF CPUX64}
    Registers: array[paEDX..paECX] of Cardinal;
    EAXRegister: Cardinal;
    ReturnAddress: Pointer;
{$ENDIF}
    Stack: array[0..1023] of Byte;
  end;
{$IFNDEF CPUX64}
asm
  push ebp
  mov ebp,esp
  push eax // ebp-4 = Method
  push ebx
  mov ebx, edx // ebx = Parameters

  // if StackSize > 0
  test ecx,ecx
  jz @@no_stack

  // stack address alignment
  {TODO -o##jwp -cOSX32/MACOS : Research 16-byte stack alignment: http://docwiki.embarcadero.com/RADStudio/XE5/en/Delphi_Considerations_for_Cross-Platform_Applications#Stack_Alignment_Issue_on_OS_X }
  // http://docwiki.embarcadero.com/RADStudio/XE5/en/Conditional_compilation_(Delphi)
  add ecx,PointerSize-1
  and ecx,not(PointerSize-1)
  and ecx,$ffff
  sub esp,ecx

  // put stack address as second parameter
  mov edx,esp

  // put params on stack as first parameter
  lea eax,[ebx].TParameters.Stack

  call Move

@@no_stack:
  mov edx,[ebx].TParameters.Registers.dword[0]
  mov ecx,[ebx].TParameters.Registers.dword[4]
  mov ebx,[ebp-$04]
  mov eax,[ebx].TMethod.Data
  call [ebx].TMethod.Code

  pop ebx
  pop eax
  mov esp,ebp
  pop ebp
end;
{$ELSE}
asm
  .params 60
  mov [rbp+$200],Method
  mov [rbp+$208],Parameters

  // put params on stack as first parameter
  mov rcx,Parameters

  // put stack address as second parameter
  mov rdx,rbp

  call Move

  mov rax,[rbp+$208]

  mov rcx,[rax].TParameters.Stack.qword[0]
  mov rdx,[rax].TParameters.Stack.qword[8]
  mov r8,[rax].TParameters.Stack.qword[16]
  mov r9,[rax].TParameters.Stack.qword[24]

  movsd xmm0,[rax].TParameters.Stack.qword[0]
  movsd xmm1,[rax].TParameters.Stack.qword[8]
  movsd xmm2,[rax].TParameters.Stack.qword[16]
  movsd xmm3,[rax].TParameters.Stack.qword[24]

  mov rax,[rbp+$200]
  lea rax,[rax]
  mov rcx,[rax].TMethod.Data
  call [rax].TMethod.Code
end;
{$ENDIF}

constructor TMethodInvocations.TMethodInfo.Create(typeData: PTypeData);

  function PassByRef(typeInfo: PTypeInfo; paramFlags: TParamFlags): Boolean;
  begin
    Result := (paramFlags * [pfVar, pfAddress, pfReference, pfOut] <> [])
      and not (typeInfo.Kind in [tkFloat, tkMethod, tkInt64]);
  end;

  function Align4(Value: Integer): Integer;
  begin
    {TODO -o##jwp -cOSX32/MACOS : Research 16-byte stack alignment: http://docwiki.embarcadero.com/RADStudio/XE5/en/Delphi_Considerations_for_Cross-Platform_Applications#Stack_Alignment_Issue_on_OS_X }
    // http://docwiki.embarcadero.com/RADStudio/XE5/en/Conditional_compilation_(Delphi)
    Result := (Value + 3) and not 3;
  end;

var
  P: PByte;
  I: Integer;
{$IFNDEF CPUX64}
  curReg: Integer;
  Size: Integer;
{$ENDIF}
begin
  P := AdditionalInfoOf(typeData);
  CallConvention := TCallConv(PByte(p)^);
  ParamInfos := PParameterInfos(UIntPtr(P) + 1);

  StackSize := SizeOf(Pointer); // Self in stack
{$IFNDEF CPUX64}
  curReg := paStack;
  if CallConvention = ccReg then
  begin
    curReg := paEDX;
    StackSize := 0;
  end;
{$ENDIF}

  P := @typeData^.ParamList;

  for I := 0 to typeData^.ParamCount - 1 do
  begin
    if not Assigned(ParamInfos^[I]) then
      raise EInvalidOperationException.CreateRes(@SNoTypeInfo);
{$IFNDEF CPUX64}
    if PassByRef(ParamInfos^[I]^, TParamFlags(P[0])) then
    begin
      if curReg < paStack then
        Inc(curReg)
      else
        Inc(StackSize, 4);
    end
    else
    begin
      Size := GetTypeSize(ParamInfos^[I]^);
      if (curReg < paStack) and (Size in [1, 2, 4]) and (ParamInfos^[I]^.Kind <> tkFloat) then
        Inc(curReg)
      else
        Inc(StackSize, Align4(Size));
    end;
{$ELSE}
    if I < 3 then
    begin
      if ParamInfos^[I]^.Kind = tkFloat then
        RegisterFlag := RegisterFlag or (1 shl (I + 1));
    end;
    Inc(StackSize, 8);
{$ENDIF}
    Inc(P, 1 + P[1] + 1);
    Inc(P, P[0] + 1);
  end;

{$IFDEF CPUX64}
  if StackSize < 32 then
    StackSize := 32;
{$ENDIF}
end;

constructor TMethodInvocations.Create(methodTypeData: PTypeData;
  methodInvokeEvent: TMethodInvokeEvent);
begin
  inherited Create;
  fMethodInfo := TMethodInfo.Create(methodTypeData);
  fMethodInvokeEvent := methodInvokeEvent;
end;

procedure TMethodInvocations.InternalInvokeHandlers(Params: PParameters);
begin
  if Assigned(fMethodInvokeEvent) then
    fMethodInvokeEvent(Params, fMethodInfo.StackSize);
end;

procedure TMethodInvocations.InvokeEventHandlerStub;
{$IFNDEF CPUX64}
const
  PtrSize = SizeOf(Pointer);
asm
        // is register conversion call ?
        CMP     BYTE PTR Self.fMethodInfo.CallConvention, ccReg
        JZ      @Begin
        Mov     EAX, [esp + 4]
@Begin:
        PUSH    EAX
        PUSH    ECX
        PUSH    EDX
        MOV     EDX,ESP
        CALL    InternalInvokeHandlers
        // Pop EDX and ECX off the stack while preserving all registers.
        MOV     [ESP+4],EAX
        POP     EAX
        POP     EAX
        POP     ECX		// Self
        Mov     EAX, ECX
        MOV     ECX,[ECX].fMethodInfo.StackSize
        TEST    ECX,ECX
        JZ      @@SimpleRet
        // Jump to the actual return instruction since it is most likely not just a RET
        //JMP     ECX    // Data Exec. Prevention: Jumping into a GetMem allocated memory block

        // stack address alignment
        {TODO -o##jwp -cOSX32/MACOS : Research 16-byte stack alignment: http://docwiki.embarcadero.com/RADStudio/XE5/en/Delphi_Considerations_for_Cross-Platform_Applications#Stack_Alignment_Issue_on_OS_X }
        // http://docwiki.embarcadero.com/RADStudio/XE5/en/Conditional_compilation_(Delphi)
        // In cdecl call conversion, the caller will clear the stack
        CMP     DWORD PTR [EAX].fMethodInfo.CallConvention, ccCdecl
        JZ      @@SimpleRet
        ADD     ECX, PtrSize - 1
        AND     ECX, NOT (PtrSize - 1)
        AND     ECX, $FFFF

        // clean up the stack
        PUSH    EAX                         // we need this register, so save it
        MOV     EAX,[ESP + 4]               // Load the return address
        MOV     [ESP + ECX + 4], EAX        // Just blast it over the first param on the stack
        POP     EAX
        ADD     ESP,ECX                     // This will move the stack back to where the moved
                                            // return address is now located. The next RET
                                            // instruction will do the final stack cleanup
@@SimpleRet:
end;
{$ELSE}
asm
        .PARAMS 2
        MOV     AX, WORD PTR [RCX].TMethodInvocations.fMethodInfo.RegisterFlag
@@FIRST:
        TEST    AX, $01
        JZ      @@SAVE_RCX
@@SAVE_XMM0:
        MOVSD   QWORD PTR [RSP+$30], XMM0
        JMP     @@SECOND
@@SAVE_RCX:
        MOV     QWORD PTR [RSP+$30], RCX

@@SECOND:
        TEST    AX, $02
        JZ      @@SAVE_RDX
@@SAVE_XMM1:
        MOVSD   QWORD PTR [RSP+$38], XMM1
        JMP     @@THIRD
@@SAVE_RDX:
        MOV     QWORD PTR [RSP+$38], RDX

@@THIRD:
        TEST    AX, $04
        JZ      @@SAVE_R8
@@SAVE_XMM2:
        MOVSD   QWORD PTR [RSP+$40], XMM2
        JMP     @@FORTH
@@SAVE_R8:
        MOV     QWORD PTR [RSP+$40], R8

@@FORTH:
        TEST    AX, $08
        JZ      @@SAVE_R9
@@SAVE_XMM3:
        MOVSD   QWORD PTR [RSP+$48], XMM3
        JMP     @@1
@@SAVE_R9:
        MOV     QWORD PTR [RSP+$48], R9

@@1:    LEA     RDX, QWORD PTR [RSP+$30]
        CALL    InternalInvokeHandlers
end;
{$ENDIF}
{$ENDIF}

{$ENDREGION}


{$REGION 'TEvent'}

constructor TEvent.Create(typeInfo: PTypeInfo);
var
  method: TRttiMethod;
{$IFNDEF PUREPASCAL}
  typeData: PTypeData;
{$ENDIF}
begin
  fTypeInfo := typeInfo;
  if not Assigned(typeInfo) then
    raise EInvalidOperationException.CreateRes(@SNoTypeInfo);

  inherited Create;

  case typeInfo.Kind of
    tkMethod:
    begin
{$IFDEF PUREPASCAL}
      TMethodImplementation(fProxy) := TRttiInvokableType(typeInfo.RttiType)
        .CreateImplementation(nil, InternalInvokeMethod);
{$IFDEF AUTOREFCOUNT}
      // Release reference created by passing closure to InternalInvokeMethod (RSP-10176)
      __ObjRelease;
{$ENDIF}
      TMethod(fInvoke) := TMethodImplementation(fProxy).AsMethod;
{$ELSE}
      typeData := typeInfo.TypeData;
      fInvocations := TMethodInvocations.Create(typeData, InternalInvoke);
      fInvoke := TMethodInvocations(fInvocations).InvokeEventHandlerStub;
{$ENDIF}
    end;
    tkInterface:
    begin
      method := typeInfo.RttiType.GetMethod('Invoke');
      if not Assigned(method) then
        raise EInvalidOperationException.CreateResFmt(@STypeParameterContainsNoRtti, [typeInfo.Name]);
{$IFDEF PUREPASCAL}
      TVirtualInterface.Create(typeInfo, InternalInvokeDelegate)
        .QueryInterface(typeInfo.TypeData.Guid, fProxy);
{$IFDEF AUTOREFCOUNT}
      // Release reference held by TVirtualInterface.RawCallBack (bypass RSP-10177)
      IInterface(fProxy)._Release;
      // Release reference created by passing closure to InternalInvokeDelegate (RSP-10176)
      __ObjRelease;
{$ENDIF}
{$ELSE}
      New(typeData);
      try
        GetMethodTypeData(method, typeData);
        fInvocations := TMethodInvocations.Create(typeData, InternalInvoke);
        fInvoke := TMethodInvocations(fInvocations).InvokeEventHandlerStub;
      finally
        Dispose(typeData);
      end;
{$ENDIF}
    end
  else
    raise EInvalidOperationException.CreateResFmt(@STypeParameterShouldBeMethod, [typeInfo.Name]);
  end;
end;

destructor TEvent.Destroy;
begin
{$IFDEF PUREPASCAL}
  case fTypeInfo.Kind of
    tkMethod: TMethodImplementation(fProxy).Free;
    tkInterface: IInterface(fProxy) := nil;
  end;
{$ELSE}
  fInvocations.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF PUREPASCAL}
procedure TEvent.InternalInvokeMethod(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
var
  handler: TMethodPointer;
  argsWithoutSelf: TArray<TValue>;
  value: TValue;
begin
  if CanInvoke then
  begin
    argsWithoutSelf := Copy(Args, 1);
    for handler in Handlers do
    begin
      TValue.Make(@TMethod(handler), TRttiInvokableType(UserData).Handle, value);
      TRttiInvokableType(UserData).Invoke(value, argsWithoutSelf);
    end;
  end;
end;

procedure TEvent.InternalInvokeDelegate(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  handler: TMethodPointer;
  reference: IInterface;
  argsWithoutSelf: TArray<TValue>;
  value: TValue;
begin
  if CanInvoke then
  begin
    argsWithoutSelf := Copy(Args, 1);
    for handler in Handlers do
    begin
      reference := MethodPointerToMethodReference(handler);
      TValue.Make(@reference, TypeInfo(IInterface), value);
      method.Invoke(value, argsWithoutSelf);
    end;
  end;
end;
{$ELSE}
procedure TEvent.InternalInvoke(Params: Pointer; StackSize: Integer);
var
  handler: TMethodPointer;
begin
  if CanInvoke then
    for handler in Handlers do
      InvokeMethod(TMethod(handler), Params, StackSize);
end;

procedure TEvent.Invoke;
asm
{$IFDEF CPUX64}
  mov rax,[rcx].fInvoke.TMethod.Code
  mov rcx,[rcx].fInvoke.TMethod.Data
  jmp rax
{$ELSE}
  push [eax].fInvoke.TMethod.Code
  mov eax,[eax].fInvoke.TMethod.Data
{$ENDIF}
end;
{$ENDIF}

procedure TEvent.Notify(Sender: TObject; const Item: TMethodPointer;
  Action: TEventBase.TCollectionNotification);
begin
  inherited Notify(Sender, Item, Action);
  if fTypeInfo.Kind = tkInterface then
    case Action of
      cnAdded: IInterface(TMethod(Item).Data)._AddRef;
      cnRemoved: IInterface(TMethod(Item).Data)._Release;
    end;
end;

{$ENDREGION}


{$REGION 'TEvent<T>'}

constructor TEvent<T>.Create;
begin
  inherited Create(TypeInfo(T));
end;

procedure TEvent<T>.Add(handler: T);
begin
  if TType.Kind<T> = tkInterface then
    inherited Add(MethodReferenceToMethodPointer(handler))
  else
    inherited Add(PMethodPointer(@handler)^);
end;

procedure TEvent<T>.Remove(handler: T);
begin
  if TType.Kind<T> = tkInterface then
    inherited Remove(MethodReferenceToMethodPointer(handler))
  else
    inherited Remove(PMethodPointer(@handler)^);
end;

function TEvent<T>.GetInvoke: T;
begin
  if TType.Kind<T> = tkInterface then
{$IFDEF PUREPASCAL}
    PInterface(@Result)^ := IInterface(fProxy)
{$ELSE}
    TProc(PPointer(@Result)^) := Self
{$ENDIF}
  else
    PMethodPointer(@Result)^ := fInvoke;
end;

{$ENDREGION}


{$REGION 'TNotifyEventImpl'}

constructor TNotifyEventImpl.Create;
begin
  inherited Create;
  TNotifyEvent(fInvoke) := InternalInvoke;
end;

procedure TNotifyEventImpl.Add(handler: TNotifyEvent);
begin
  inherited Add(TMethodPointer(handler));
end;

function TNotifyEventImpl.GetInvoke: TNotifyEvent;
begin
  Result := TNotifyEvent(inherited Invoke);
end;

procedure TNotifyEventImpl.InternalInvoke(sender: TObject);
var
  handler: TMethodPointer;
begin
  if Enabled then
    for handler in Handlers do
      TNotifyEvent(handler)(sender);
end;

procedure TNotifyEventImpl.Remove(handler: TNotifyEvent);
begin
  inherited Remove(TMethodPointer(handler));
end;

{$ENDREGION}


{$REGION 'TNotifyEventImpl<T>'}

constructor TNotifyEventImpl<T>.Create;
begin
  inherited Create;
  TNotifyEvent<T>(fInvoke) := InternalInvoke;
end;

procedure TNotifyEventImpl<T>.Add(handler: TNotifyEvent<T>);
begin
  inherited Add(TMethodPointer(handler));
end;

function TNotifyEventImpl<T>.GetInvoke: TNotifyEvent<T>;
begin
  Result := TNotifyEvent<T>(inherited Invoke);
end;

procedure TNotifyEventImpl<T>.InternalInvoke(sender: TObject; const item: T);
var
  handler: TMethodPointer;
begin
  if Enabled then
    for handler in Handlers do
      TNotifyEvent<T>(handler)(sender, item);
end;

procedure TNotifyEventImpl<T>.Remove(handler: TNotifyEvent<T>);
begin
  inherited Remove(TMethodPointer(handler));
end;

{$ENDREGION}


{$REGION 'TPropertyChangedEventImpl'}

constructor TPropertyChangedEventImpl.Create;
begin
  inherited Create;
  TPropertyChangedEvent(fInvoke) := InternalInvoke;
end;

procedure TPropertyChangedEventImpl.Add(handler: TPropertyChangedEvent);
begin
  inherited Add(TMethodPointer(handler));
end;

function TPropertyChangedEventImpl.GetInvoke: TPropertyChangedEvent;
begin
  Result := TPropertyChangedEvent(inherited Invoke);
end;

procedure TPropertyChangedEventImpl.InternalInvoke(Sender: TObject;
  const EventArgs: IPropertyChangedEventArgs);
var
  handler: TMethodPointer;
begin
  if Enabled then
    for handler in Handlers do
      TPropertyChangedEvent(handler)(Sender, EventArgs);
end;

procedure TPropertyChangedEventImpl.Remove(handler: TPropertyChangedEvent);
begin
  inherited Remove(TMethodPointer(handler));
end;

{$ENDREGION}


{$REGION 'TMethodEvent'}

procedure TMethodEvent.Add(handler: TMethodPointer);
begin
  inherited Add(handler);
end;

procedure TMethodEvent.Remove(handler: TMethodPointer);
begin
  inherited Remove(handler);
end;

{$ENDREGION}


{$REGION 'TDelegateEvent'}

function TDelegateEvent.GetInvoke: IInterface;
begin
{$IFDEF PUREPASCAL}
  PInterface(@Result)^ := IInterface(fProxy);
{$ELSE}
  TProc(PPointer(@Result)^) := Self;
{$ENDIF}
end;

procedure TDelegateEvent.Add(handler: IInterface);
begin
  inherited Add(MethodReferenceToMethodPointer(handler));
end;

procedure TDelegateEvent.Remove(handler: IInterface);
begin
  inherited Remove(MethodReferenceToMethodPointer(handler));
end;

{$ENDREGION}


end.
