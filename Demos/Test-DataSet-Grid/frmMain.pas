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

  System.Bindings.Outputs,

  Fmx.Bind.GenData,
  Fmx.Bind.Editors,
  Fmx.Bind.DBEngExt,
  Fmx.Bind.Navigator,
  Fmx.Bind.Grid,

  Data.Bind.GenData,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  Data.Bind.EngExt,
  Data.Bind.DBScope,
  Data.Bind.Controls,
  Data.Bind.Grid,

  Data.DB,
  Datasnap.DBClient,

  System.Generics.Collections;

type
  TfrmMain = class(TForm)
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    BindSourceDB1: TBindSourceDB;
    cdsSource: TClientDataSet;
    cdsSourceSpeciesNo: TFloatField;
    cdsSourceCategory: TStringField;
    cdsSourceCommon_Name: TStringField;
    cdsSourceSpeciesName: TStringField;
    cdsSourceLengthcm: TFloatField;
    cdsSourceLength_In: TFloatField;
    cdsSourceNotes: TMemoField;
    cdsSourceGraphic: TGraphicField;
    BindingsList1: TBindingsList;
    Grid1: TGrid;
    BindNavigator1: TBindNavigator;
    Button7: TButton;
    Button5: TButton;
    Button4: TButton;
    Image1: TImage;
    Label1: TLabel;
    InfoLabel: TLabel;
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  end;

var
  fMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  TBinder.BindDataSetToGrid(cdsSource, Grid1);
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  TBinder.ClearDataSetBindingFromGrid(Grid1);
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
    LSource          := TBindSourceDB.Create(AGrid);
    LSource.DataSet  := ADataSet;
  end;
  if not LExisteL then
  begin
    LGridLinker          := TLinkGridToDataSource.Create(LSource);
    LGridLinker.Category := 'Quick Bindings';
    LGridLinker.GridControl  := AGrid;

    LGridLinker.DataSource := LSource;
  end;
  for I := Low(AColumnLinks) to High(AColumnLinks) do
  begin
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
  LGridLinker.Active   := True;
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
