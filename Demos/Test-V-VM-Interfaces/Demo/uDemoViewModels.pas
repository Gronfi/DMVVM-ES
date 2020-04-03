unit uDemoViewModels;

interface

uses
  uInterfaces,
  uDemoInterfaces;

type
  TMyViewModel1 = class(TViewModel, IMyViewModel1, IViewModel)
    public
      function Hello1: String;
  end;

  TMyViewModel2 = class(TViewModel, IMyViewModel2, IViewModel)
    public
      function Hello2: String;
  end;

  TMyViewModel3 = class(TViewModel, IMyViewModel3, IViewModel)
    private
      FVM1: IMyViewModel1;
      FVM2: IMyViewModel2;
    protected
      function GetVM_Tipo1: IMyViewModel1;
      function GetVM_Tipo2: IMyViewModel2;
      procedure SetVM_Tipo1(AValue: IMyViewModel1);
      procedure SetVM_Tipo2(AValue: IMyViewModel2);
    public
      function Hello: String;

      property VM_Tipo1: IMyViewModel1 read GetVM_Tipo1 write SetVM_Tipo1;
      property VM_Tipo2: IMyViewModel2 read GetVM_Tipo2 write SetVM_Tipo2;
  end;

implementation

{ TMyViewModel1 }

function TMyViewModel1.Hello1: String;
begin
  Result := self.QualifiedClassName
end;

{ TMyViewModel2 }

function TMyViewModel2.Hello2: String;
begin
  Result := self.QualifiedClassName
end;

{ TMyViewModel3 }

function TMyViewModel3.GetVM_Tipo1: IMyViewModel1;
begin
  Result := FVM1;
end;

function TMyViewModel3.GetVM_Tipo2: IMyViewModel2;
begin
  Result := FVM2;
end;

function TMyViewModel3.Hello: String;
begin
  Result := FVM1.Hello1 + ' - ' + FVM2.Hello2;
end;

procedure TMyViewModel3.SetVM_Tipo1(AValue: IMyViewModel1);
begin
  FVM1 := AValue;
end;

procedure TMyViewModel3.SetVM_Tipo2(AValue: IMyViewModel2);
begin
  FVM2 := AValue;
end;

end.
