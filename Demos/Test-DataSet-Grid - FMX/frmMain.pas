unit frmMain;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Types, System.UITypes,
  System.Rtti,

  FMX.Forms, FMX.Dialogs, FMX.Types, FMX.Layouts, FMX.Styles, FMX.StdCtrls,
  FMX.Objects, FMX.Controls, FMX.Edit, FMX.Effects, FMX.Graphics,
  FMX.Controls.Presentation,
  FMX.ScrollBox,

  FMX.Grid, FMX.Grid.Style,

  System.Generics.Collections,

  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  Fmx.Bind.Grid,
  Fmx.Bind.Editors,

  Data.Bind.Grid,
  Data.Bind.DBScope,
  Data.Bind.Controls,
  Fmx.Bind.Navigator, Datasnap.DBClient, FMX.Memo;

type
  TfrmMain = class(TForm)
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    Grid1: TGrid;
    BindNavigator1: TBindNavigator;
    Button7: TButton;
    Button5: TButton;
    Button4: TButton;
    Image1: TImage;
    Label1: TLabel;
    InfoLabel: TLabel;
    Button1: TButton;
    cdsSource: TClientDataSet;
    cdsSourceSpeciesNo: TFloatField;
    cdsSourceCategory: TStringField;
    cdsSourceCommon_Name: TStringField;
    cdsSourceSpeciesName: TStringField;
    cdsSourceLengthcm: TFloatField;
    cdsSourceLength_In: TFloatField;
    cdsSourceNotes: TMemoField;
    cdsSourceGraphic: TGraphicField;
    Memo1: TMemo;
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TBinder = class
  type
    TColumnConfig = record
    public
      DSField: String;
      GHeader: String;
      ReadOnly: Boolean;
      Width: Integer;
      Visible: Boolean;
      CustomFormat: String;
      CustomParse: String;
      ColumnStyle: String;
      constructor Create(const ADSField: String; const AGHeader: String; const AReadOnly: Boolean; const AWidth: Integer; const AVisible: Boolean;
                         const ACustomFormat: String; const ACustomParse: String; const AColumnStyle: String);
    end;
  public
    class function ClearDataSetBindingFromGrid(AGrid: TCustomGrid): Boolean; static;
    class function DisableBindingFromGrid(AGrid: TCustomGrid): Boolean; static;
    class function EnableBindingFromGrid(AGrid: TCustomGrid; ADataSet: TDataSet): Boolean; static;
    class procedure BindDataSetToGrid(ADataSet: TDataSet; AGrid: TCustomGrid); overload; static;
    class procedure BindDataSetToGrid(ADataSet: TDataSet; AGrid: TCustomGrid; const AColumnLinks: array of TColumnConfig); overload; static;
    class procedure RemoveBindingDataSetColumnsFromGrid( AGrid: TCustomGrid; const AColumns: array of String); overload; static;
  end;

var
  fMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  TBinder.RemoveBindingDataSetColumnsFromGrid(Grid1, ['Species No'])
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  TBinder.BindDataSetToGrid(cdsSource, Grid1);
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  //TBinder.ClearDataSetBindingFromGrid(Grid1);
  TBinder.BindDataSetToGrid(cdsSource, Grid1,
                            [
                              TBinder.TColumnConfig.Create('Common_Name', 'Common Name', False, 200, True, '%s + '' - '' + DataSet.Category.Text', '', ''),
                              TBinder.TColumnConfig.Create('Species No', 'Species No', True, 100, True, '', '', ''),
                              TBinder.TColumnConfig.Create('Length_In', 'Length In', True, 100, True, '', '', 'ProgressColumn')
                            ]
                           );
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
  TBinder.BindDataSetToGrid(cdsSource, Grid1,
  [
    TBinder.TColumnConfig.Create('Graphic', 'Bitmap', False, 100, True, '', '', '')
  ]);
end;

procedure TfrmMain.CheckBox1Change(Sender: TObject);
begin
  case TCheckBox(Sender).IsChecked of
    True:
      begin
        Timer1.Enabled := True;
      end;
    False:
      begin
        Timer1.Enabled := False;
      end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Randomize;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
var
  LActual: Integer;
begin
  LActual := cdsSource.RecNo;
  TBinder.DisableBindingFromGrid(Grid1);
  try
    cdsSource.First;
    while not cdsSource.Eof do
    begin
      cdsSource.Edit;
      cdsSource.FieldByName('Length_In').AsInteger := (cdsSource.FieldByName('Length_In').AsInteger + Random(20)) MOD 100 ;
      cdsSource.Post;

      cdsSource.Next;
    end;
  finally
    cdsSource.RecNo := LActual;
    TBinder.EnableBindingFromGrid(Grid1, cdsSource);
  end;
end;

class procedure TBinder.BindDataSetToGrid(ADataSet: TDataSet; AGrid: TCustomGrid);
var
  LLinker: TLinkGridToDataSource;
  LSource: TBindSourceDB;
begin
  ClearDataSetBindingFromGrid(AGrid);

  LSource          := TBindSourceDB.Create(AGrid);
  LSource.DataSet  := ADataSet;
  LLinker          := TLinkGridToDataSource.Create(LSource);
  LLinker.Category := 'Quick Bindings';
  LLinker.GridControl  := AGrid;

  LLinker.DataSource := LSource;
  LLinker.Active   := True;
end;

class procedure TBinder.BindDataSetToGrid(ADataSet: TDataSet; AGrid: TCustomGrid; const AColumnLinks: array of TColumnConfig);
var
  LGridLinker: TLinkGridToDataSource;
  LColumnLinker: TLinkGridToDataSourceColumn;
  LSource: TBindSourceDB;
  I, J      : Integer;
  LExisteS, LExisteL, LExisteC: Boolean;
  LDebug: TStringList;
begin
  fMain.Memo1.lines.Clear;
  LDebug := TStringList.Create;
  try
    //mirar si existe o no un linkado previo
    LExisteS := False;
    LExisteL := False;
    LDebug.add('STEP0');
    for I := 0 to AGrid.ComponentCount - 1 do
    begin
      LDebug.add('STEP1: ' + AGrid.Components[I].QualifiedClassName);
      if (AGrid.Components[I] is TBindSourceDB) then
      begin
        LDebug.add('STEP2');
        LExisteS := True;
        LSource := AGrid.Components[I] as TBindSourceDB;
        for J := 0 to AGrid.Components[I].ComponentCount - 1 do
        begin
          LDebug.add('STEP3: ' + AGrid.Components[I].Components[J].QualifiedClassName);
          if (AGrid.Components[I].Components[J] is TLinkGridToDataSource) then
          begin
            LDebug.add('STEP4');
            LGridLinker := AGrid.Components[I].Components[J] as TLinkGridToDataSource;
            LGridLinker.Active := False;
            LExisteL := True;
            Break;
          end;
        end;
        break;
      end;
    end;
    LDebug.add('STEP5');
    if not LExisteS then
    begin
      LDebug.add('STEP6: crea TBindSourceDB');
      LSource          := TBindSourceDB.Create(AGrid);
      LSource.DataSet  := ADataSet;
    end;
    if not LExisteL then
    begin
      LDebug.add('STEP7: crea TLinkGridToDataSource');
      LGridLinker          := TLinkGridToDataSource.Create(LSource);
      LGridLinker.Category := 'Quick Bindings';
      LGridLinker.GridControl  := AGrid;

      LGridLinker.DataSource := LSource;
    end;
    LDebug.add('STEP8');
    for I := Low(AColumnLinks) to High(AColumnLinks) do
    begin
      LExisteC := False;
      for J := 0 to LGridLinker.Columns.Count - 1 do
      begin
        if LGridLinker.Columns.Items[J].Header = AColumnLinks[I].GHeader then
        begin
          LDebug.add('STEP9: ' + AColumnLinks[I].GHeader);
          LExisteC := True;
          Break;
        end;
      end;
      LDebug.add('STEP10');
      if not LExisteC then
      begin
        LDebug.add('STEP11');
        LColumnLinker := LGridLinker.Columns.Add;
        LColumnLinker.MemberName := AColumnLinks[I].DSField;
        LColumnLinker.Header     := AColumnLinks[I].GHeader;
        LColumnLinker.ReadOnly   := AColumnLinks[I].ReadOnly;
        LColumnLinker.Width      := AColumnLinks[I].Width;
        LColumnLinker.Visible    := AColumnLinks[I].Visible;
        LColumnLinker.CustomFormat := AColumnLinks[I].CustomFormat;
        LColumnLinker.CustomParse  := AColumnLinks[I].CustomParse;
        LColumnLinker.ColumnStyle  := AColumnLinks[I].ColumnStyle;
      end;
    end;
    LDebug.add('STEP12');
    LGridLinker.Active   := True;
    fMain.Memo1.lines.add(LDebug.Text);
  finally
    LDebug.Free;
  end;
end;

class function TBinder.ClearDataSetBindingFromGrid(AGrid: TCustomGrid): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AGrid.ComponentCount - 1 do
  begin
    if (AGrid.Components[I] is TBindSourceDB) then
    begin
      AGrid.Components[I].Free;
      Exit(True);
    end;
  end;
end;

class function TBinder.DisableBindingFromGrid(AGrid: TCustomGrid): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AGrid.ComponentCount - 1 do
  begin
    if (AGrid.Components[I] is TBindSourceDB) then
    begin
      TBindSourceDB(AGrid.Components[I]).DataSet := nil;
      Exit(True);
    end;
  end;
end;

class function TBinder.EnableBindingFromGrid(AGrid: TCustomGrid; ADataSet: TDataSet): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AGrid.ComponentCount - 1 do
  begin
    if (AGrid.Components[I] is TBindSourceDB) then
    begin
      TBindSourceDB(AGrid.Components[I]).DataSet := ADataSet;
      Exit(True);
    end;
  end;
end;

class procedure TBinder.RemoveBindingDataSetColumnsFromGrid(AGrid: TCustomGrid; const AColumns: array of String);
var
  LGridLinker: TLinkGridToDataSource;
  LColumnLinker: TLinkGridToDataSourceColumn;
  LSource: TBindSourceDB;
  I, J      : Integer;
  LExisteS, LExisteL: Boolean;
begin
  //mirar si existe o no un linkado previo
  LExisteS := False;
  LExisteL := False;
  for I := 0 to AGrid.ComponentCount - 1 do
  begin
    if (AGrid.Components[I] is TBindSourceDB) then
    begin
      LExisteS := True;
      LSource := AGrid.Components[I] as TBindSourceDB;
      for J := 0 to AGrid.Components[I].ComponentCount - 1 do
      begin
        if (AGrid.Components[I].Components[J] is TLinkGridToDataSource) then
        begin
          LGridLinker := AGrid.Components[I].Components[J] as TLinkGridToDataSource;
          LGridLinker.Active := False;
          LExisteL := True;
          Break;
        end;
      end;
      if LExisteL then
        break;
    end;
  end;
  if not LExisteS then
  begin
    Exit;
  end;
  if not LExisteL then
  begin
    Exit;
  end;
  for J := Low(AColumns) to High(AColumns) do
  begin
    for I := LGridLinker.Columns.Count - 1 downto 0 do
    begin
      if LGridLinker.Columns.Items[I].Header = AColumns[J] then
      begin
        LGridLinker.Columns.Delete(I);
        Break;
      end;
    end;
  end;
  LGridLinker.Active   := True;
end;

constructor TBinder.TColumnConfig.Create(const ADSField: String; const AGHeader: String; const AReadOnly: Boolean; const AWidth: Integer; const AVisible: Boolean;
                         const ACustomFormat: String; const ACustomParse: String; const AColumnStyle: String);
begin
  DSField      := ADSField;
  GHeader       := AGHeader;
  CustomFormat := ACustomFormat;
  ReadOnly     := AReadOnly;
  Width := AWidth;
  Visible := AVisible;
  CustomFormat := ACustomFormat;
  CustomParse := ACustomParse;
  ColumnStyle := AColumnStyle;
end;

end.
