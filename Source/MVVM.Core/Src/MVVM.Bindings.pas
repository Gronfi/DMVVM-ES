unit MVVM.Bindings;

interface

uses
  System.Classes,
  Generics.Collections,
  System.Bindings.Expression, System.Bindings.Helper,
  System.RTTI,

  Spring,
  Spring.Collections,

  MVVM.Interfaces,
  MVVM.Types;

type
  TEstrategiaBindingBase = class abstract(TInterfacedObject, IEstrategiaBinding)
  private
    FObject: TObject;
  protected
    function GetObjeto: TObject;
    procedure SetObjeto(AObjeto: TObject);
  public
    constructor Create(AObject: TObject); overload; virtual;
    destructor Destroy; override;

    procedure Start; virtual;

    procedure Notify(const APropertyName: String = ''); virtual; abstract;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String;
      const ATarget: TObject; const ATargetPropertyPath: String;
      const ADirection: EBindDirection = EBindDirection.OneWay;
      const AFlags: EBindFlags = [];
      const AValueConverterClass: TBindingValueConverterClass = nil;
      const AExtraParams: TBindExtraParams = []); virtual; abstract;

    procedure ClearBindings; virtual; abstract;

    property Objeto: TObject read GetObjeto write SetObjeto;
  end;

  TClass_EstrategiaBindingBase = class of TEstrategiaBindingBase;

  TEstrategia_LiveBindings = class(TEstrategiaBindingBase)
  protected
    type
      TExpressionList = TObjectList<TBindingExpression>;
  private
    FBindings: TExpressionList;
  protected
    property Bindings: TExpressionList read FBindings;
  public
    constructor Create(AObject: TObject); override;
    destructor Destroy; override;

    procedure Notify(const APropertyName: string = ''); override;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String;
      const ATarget: TObject; const ATargetPropertyPath: String;
      const ADirection: EBindDirection = EBindDirection.OneWay;
      const AFlags: EBindFlags = [];
      const AValueConverterClass: TBindingValueConverterClass = nil;
      const AExtraParams: TBindExtraParams = []); override;

    procedure ClearBindings; override;
  end;

type

  TBindingHelper_V2 = class
  private
    class var
      FDiccionarioEstrategiasBinding: IDictionary<String, TClass_EstrategiaBindingBase>;
  private
    var
      FObject                            : TObject;
      FDiccionarioEstrategias            : IDictionary<String, IEstrategiaBinding>;
      FDiccionarioNotificacionEstrategias: IDictionary<String, IList<String>>;
  protected
    class constructor CreateC;
    class destructor DestroyC;

    function ChequeoIntegridadSeleccionBinding(const AEstrategiaBinding: String): IEstrategiaBinding;
    function GetEstrategiaPorDefecto: String; virtual;
  public
    constructor Create(AObject: TObject); virtual;
    destructor Destroy; override;

    procedure Bind(const ASource: TObject; const ASourcePropertyPath: String;
                   const ATarget: TObject; const ATargetPropertyPath: String;
                   const ADirection: EBindDirection = EBindDirection.OneWay;
                   const AFlags: EBindFlags = [];
                   const AValueConverterClass: TBindingValueConverterClass = nil;
                   const AEstrategiaBinding: String = '';
                   const AExtraParams: TBindExtraParams = []);

    procedure Notify(const APropertyName: String);

    (*
    procedure BindCollection<T: class>(const ACollection: IEnumerable<T>;
      const ATarget: IgoCollectionViewProvider;
      const ATemplate: TgoDataTemplateClass;
      const AEstrategiaBinding: String = '');

    procedure BindAction(const AAction: IgoBindableAction;
      const AExecute: TgoExecuteMethod;
      const ACanExecute: TgoCanExecuteMethod = nil;
      const AEstrategiaBinding: String = ''); overload; inline;
    procedure BindAction(const AAction: IgoBindableAction;
      const AExecute: TgoExecuteMethod<Integer>;
      const ACanExecute: TgoCanExecuteMethod = nil;
      const AEstrategiaBinding: String = ''); overload; inline;
    *)
    class procedure RegistrarEstrategiaBinding(const AEstrategia: String; AEstrategiaBindingClass: TClass_EstrategiaBindingBase);
  end;

  (*
  TBindingHelper = class
  protected
    type
      TExpressionList = TObjectList<TBindingExpression>;
  private
    FObject: TObject;
    FBindings: TExpressionList;
  protected
    property Bindings: TExpressionList read FBindings;
  public
    constructor Create(AObject: TObject); virtual;
    destructor Destroy; override;

    procedure Notify(const APropertyName: string = '');

    procedure Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string); overload;
    procedure BindReverse(const ABindObject: TObject; const AProperty: string; const ABindToProperty: string); overload;

    procedure Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string); overload;
    procedure BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression: string; const ADstAlias, ADstFormatedExpression: string); overload;

    class function CreateEvent<T>: IEvent<T>; static;

    procedure ClearBindings;
  end;
  *)

implementation

uses
  System.SysUtils;

(*
constructor TBindingHelper.Create(AObject: TObject);
begin
  inherited Create;
  FObject   := AObject;
  FBindings := TExpressionList.Create(false {AOwnsObjects});
end;

class function TBindingHelper.CreateEvent<T>: IEvent<T>;
var
  E: Event<T>;
begin
  Result := E;
end;

destructor TBindingHelper.Destroy;
begin
  ClearBindings;
  FBindings.Free;
  inherited;
end;

procedure TBindingHelper.ClearBindings;
var
  i: TBindingExpression;
begin
  for i in FBindings do
    TBindings.RemoveBinding(i);
  FBindings.Clear;
end;

procedure TBindingHelper.Notify(const APropertyName: string);
begin
  TBindings.Notify(FObject, APropertyName);
end;

procedure TBindingHelper.Bind(const AProperty: string; const ABindToObject: TObject; const ABindToProperty: string);
begin
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(FObject, 'src')])],
      'src.' + AProperty,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(ABindToObject, 'dst')])],
      'dst.' + ABindToProperty,
      nil, nil, [coNotifyOutput, coEvaluate]));
end;

procedure TBindingHelper.Bind(const ASrcAlias, ASrcFormatedExpression: string; const ABindToObject: TObject; const ADstAlias, ADstFormatedExpression: string);
begin
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(FObject, ASrcAlias)])],
      ASrcFormatedExpression,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(ABindToObject, ADstAlias)])],
      ADstFormatedExpression,
      nil, nil, [coNotifyOutput, coEvaluate]));
end;

procedure TBindingHelper.BindReverse(const ABindObject: TObject; const ASrcAlias, ASrcFormatedExpression, ADstAlias, ADstFormatedExpression: string);
begin
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(ABindObject, ASrcAlias)])],
      ASrcFormatedExpression,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(FObject, ADstAlias)])],
      ADstFormatedExpression,
      nil, nil, [coNotifyOutput, coEvaluate]));
end;

procedure TBindingHelper.BindReverse(const ABindObject: TObject; const AProperty, ABindToProperty: string);
begin
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(ABindObject, 'src')])],
      'src.' + AProperty,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(FObject, 'dst')])],
      'dst.' + ABindToProperty,
      nil, nil, [coNotifyOutput, coEvaluate]));
end;
*)

{ TBindingHelper_V2 }

procedure TBindingHelper_V2.Bind(const ASource: TObject;
                                 const ASourcePropertyPath: String; const ATarget: TObject;
                                 const ATargetPropertyPath: String; const ADirection: EBindDirection;
                                 const AFlags: EBindFlags;
                                 const AValueConverterClass: TBindingValueConverterClass;
                                 const AEstrategiaBinding: String;
                                 const AExtraParams: TBindExtraParams);
var
  LEstrategia: IEstrategiaBinding;
begin
  LEstrategia := ChequeoIntegridadSeleccionBinding(AEstrategiaBinding);
  LEstrategia.Bind(ASource,
                   ASourcePropertyPath, ATarget,
                   ATargetPropertyPath, ADirection,
                   AFlags,
                   AValueConverterClass,
                   AExtraParams);
end;

function TBindingHelper_V2.ChequeoIntegridadSeleccionBinding(const AEstrategiaBinding: String): IEstrategiaBinding;
var
  LMetodo    : String;
begin
  if AEstrategiaBinding.IsEmpty then
    LMetodo := GetEstrategiaPorDefecto
  else LMetodo := AEstrategiaBinding;
  // Integridad
  Guard.CheckTrue(FDiccionarioEstrategiasBinding.ContainsKey(LMetodo), 'Estrategia de binding no registrada: ' + LMetodo);
  if not FDiccionarioEstrategias.TryGetValue(LMetodo, Result) then
  begin
    Result := FDiccionarioEstrategiasBinding[LMetodo].Create(FObject);
    FDiccionarioEstrategias.AddOrSetValue(LMetodo, Result);
  end;
end;

constructor TBindingHelper_V2.Create(AObject: TObject);
begin
  inherited Create;
  FObject                            := AObject;
  FDiccionarioNotificacionEstrategias:= TCollections.CreateDictionary<String, IList<String>>;
  FDiccionarioEstrategias            := TCollections.CreateDictionary<String, IEstrategiaBinding>;
end;

class constructor TBindingHelper_V2.CreateC;
begin
  FDiccionarioEstrategiasBinding := TCollections.CreateDictionary<String, TClass_EstrategiaBindingBase>;
end;

destructor TBindingHelper_V2.Destroy;
begin
  FDiccionarioNotificacionEstrategias := nil;
  inherited;
end;

class destructor TBindingHelper_V2.DestroyC;
begin
  FDiccionarioEstrategiasBinding := nil;
end;

function TBindingHelper_V2.GetEstrategiaPorDefecto: String;
begin
  {$IFDEF MSWINDOWS}
    Result := CEstrategiasBasicas[EEstrategiasBasicas._LIVEBINDINGS];
  {$ENDIF}
  {$IFDEF LINUX}
    Result := CEstrategiasBasicas[EEstrategiasBasicas._RTTI];
  {$ENDIF}
  {$IFDEF ANDROID}
    Result := CEstrategiasBasicas[EEstrategiasBasicas._RTTI];
  {$ENDIF}
  {$IFDEF POSIX}
    Result := CEstrategiasBasicas[EEstrategiasBasicas._RTTI];
  {$ENDIF}
  {$IFDEF MACOS}
    Result := CEstrategiasBasicas[EEstrategiasBasicas._RTTI];
  {$ENDIF}
end;

procedure TBindingHelper_V2.Notify(const APropertyName: String);
var
  LLista     : IList<String>;
  LEstrategia: String;
begin
  if FDiccionarioNotificacionEstrategias.TryGetValue(APropertyName, LLista) then
  begin
    for LEstrategia in LLista do
      FDiccionarioEstrategias[LEstrategia].Notify(APropertyName);
  end;
end;

class procedure TBindingHelper_V2.RegistrarEstrategiaBinding(const AEstrategia: String; AEstrategiaBindingClass: TClass_EstrategiaBindingBase);
begin
  FDiccionarioEstrategiasBinding.AddOrSetValue(AEstrategia, AEstrategiaBindingClass);
end;

{ TEstrategia_LiveBindings }

procedure TEstrategia_LiveBindings.Bind(const ASource: TObject; const ASourcePropertyPath: String;
                                        const ATarget: TObject; const ATargetPropertyPath: String;
                                        const ADirection: EBindDirection;
                                        const AFlags: EBindFlags;
                                        const AValueConverterClass: TBindingValueConverterClass;
                                        const AExtraParams: TBindExtraParams);
var
  LOptions: TBindings.TCreateOptions;
begin
  LOptions := [coNotifyOutput];
  if not(DontApply in AFlags) then
    LOptions := LOptions + [coEvaluate];
  FBindings.Add(TBindings.CreateManagedBinding(
      { inputs }
      [TBindings.CreateAssociationScope([Associate(ASource, 'src')])],
      'src.' + ASourcePropertyPath,
      { outputs }
      [TBindings.CreateAssociationScope([Associate(ATarget, 'dst')])],
      'dst.' + ATargetPropertyPath,
      nil,
      nil,
      LOptions));
  if ADirection = EBindDirection.TwoWay then
  begin
    FBindings.Add(TBindings.CreateManagedBinding(
        { inputs }
        [TBindings.CreateAssociationScope([Associate(ATarget, 'src')])],
        'src.' + ATargetPropertyPath,
        { outputs }
        [TBindings.CreateAssociationScope([Associate(ASource, 'dst')])],
        'dst.' + ASourcePropertyPath,
        nil,
        nil,
        LOptions));
  end;
end;

procedure TEstrategia_LiveBindings.ClearBindings;
var
  i: TBindingExpression;
begin
  for i in FBindings do
    TBindings.RemoveBinding(i);
  FBindings.Clear;
end;

constructor TEstrategia_LiveBindings.Create(AObject: TObject);
begin
  inherited;
  FBindings := TExpressionList.Create(false {AOwnsObjects});
end;

destructor TEstrategia_LiveBindings.Destroy;
begin
  ClearBindings;
  FBindings.Free;
  inherited;
end;

procedure TEstrategia_LiveBindings.Notify(const APropertyName: string);
begin
  TBindings.Notify(FObject, APropertyName);
end;

{ TEstrategiaBindingBase }

constructor TEstrategiaBindingBase.Create(AObject: TObject);
begin
  inherited Create;
  FObject := AObject;
end;

destructor TEstrategiaBindingBase.Destroy;
begin
  inherited;
end;

function TEstrategiaBindingBase.GetObjeto: TObject;
begin
  Result := FObject
end;

procedure TEstrategiaBindingBase.SetObjeto(AObjeto: TObject);
begin
  if FObject <> AObjeto then
  begin
    FObject := AObjeto;
  end;
end;

procedure TEstrategiaBindingBase.Start;
begin
  ;
end;

initialization

TBindingHelper_V2.RegistrarEstrategiaBinding(CEstrategiasBasicas[EEstrategiasBasicas._LIVEBINDINGS], TEstrategia_LiveBindings);

end.
