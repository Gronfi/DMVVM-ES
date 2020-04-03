unit uDemoInterfaces;

interface

uses
  uInterfaces;

type
  IMyViewModel1 = interface(IViewModel)
    ['{9C9E27CC-A468-4D4A-A698-3B00A0BF8847}']
    function Hello1: String;
  end;

  IMyViewModel2 = interface(IViewModel)
    ['{A199336D-D436-4C3B-9755-5B6423F264FE}']
    function Hello2: String;
  end;

  IMyViewModel3 = interface(IViewModel)
    ['{BDF3C483-9B80-4E32-95E2-81A6687B084E}']
    function GetVM_Tipo1: IMyViewModel1;
    function GetVM_Tipo2: IMyViewModel2;
    procedure SetVM_Tipo1(AValue: IMyViewModel1);
    procedure SetVM_Tipo2(AValue: IMyViewModel2);

    function Hello: String;

    property VM_Tipo1: IMyViewModel1 read GetVM_Tipo1 write SetVM_Tipo1;
    property VM_Tipo2: IMyViewModel2 read GetVM_Tipo2 write SetVM_Tipo2;
  end;

implementation

end.
