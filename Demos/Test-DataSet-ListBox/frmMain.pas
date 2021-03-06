//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit frmMain;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Types, System.UITypes,
  System.Rtti, FMX.Forms, FMX.Dialogs, FMX.Types, FMX.Layouts, FMX.Styles, FMX.StdCtrls,
  FMX.ListBox, FMX.Objects, FMX.Controls, FMX.Edit, FMX.Effects, FMX.Graphics,
  FMX.Controls.Presentation, Data.Bind.GenData, Fmx.Bind.GenData,
  Data.Bind.Components, Data.Bind.ObjectScope, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.DBScope,
  Data.DB, Datasnap.DBClient, Data.Bind.Controls, Fmx.Bind.Navigator,

  System.Generics.Collections;

type
  TfrmMain = class(TForm)
    ListBox1: TListBox;
    Resources1: TStyleBook;
    InfoLabel: TLabel;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Image2: TImage;
    BindingsList1: TBindingsList;
    cdsSource: TClientDataSet;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    BindSourceDB1: TBindSourceDB;
    BindNavigator1: TBindNavigator;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    cdsSourceSpeciesNo: TFloatField;
    cdsSourceCategory: TStringField;
    cdsSourceCommon_Name: TStringField;
    cdsSourceSpeciesName: TStringField;
    cdsSourceLengthcm: TFloatField;
    cdsSourceLength_In: TFloatField;
    cdsSourceNotes: TMemoField;
    cdsSourceGraphic: TGraphicField;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
    procedure SetNoStyled;
    procedure SetStyled;
  public
    { Public declarations }
  end;

  TBinder = class
  type
    TConvData = record
    public
      DSField: String;
      LBField: String;
      CustomFormat: String;

      constructor Create(const ADSField: String; const ALBField: String; const ACustomFormat: String = '');
    end;
  private
    class function StyledFieldOfComponent(const AField: String): String; static;
    class procedure BindDataSetToListBox(ADataSet: TDataSet; AListBox: TListbox; const AField: String; const AOnlyFillValues: Boolean = True; const ACustomDisplayExpression: String = ''); overload; static;
    class procedure BindDataSetToListBox(ADataSet: TDataSet; AListBox: TListbox; const ALinks: array of TConvData; const AOnlyFillValues: Boolean = True); overload; static;
  end;

var
  fMain: TfrmMain;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure TfrmMain.Button10Click(Sender: TObject);
begin
  SetStyled;
  TBinder.BindDataSetToListBox(cdsSource, ListBox1,
                               [
                                 TBinder.TConvData.Create('Common_Name', 'Text'),
                                 TBinder.TConvData.Create('Category', TBinder.StyledFieldOfComponent('resolution')),
                                 TBinder.TConvData.Create('Species No', TBinder.StyledFieldOfComponent('depth')),
                                 TBinder.TConvData.Create('Graphic', 'Bitmap')
                               ], False);
end;

procedure TfrmMain.Button11Click(Sender: TObject);
begin
  SetStyled;
  TBinder.BindDataSetToListBox(cdsSource, ListBox1,
                               [
                                 TBinder.TConvData.Create('Common_Name', 'Text', '%s + '' - '' + DataSet.Category.Text'),
                                 TBinder.TConvData.Create('Category', TBinder.StyledFieldOfComponent('resolution')),
                                 TBinder.TConvData.Create('Species No', TBinder.StyledFieldOfComponent('depth')),
                                 TBinder.TConvData.Create('Graphic', 'Bitmap')
                               ], False);
end;

procedure TfrmMain.SetNoStyled;
begin
  ListBox1.DefaultItemStyles.ItemStyle := '';
  ListBox1.ItemHeight                  := 20;
end;

procedure TfrmMain.SetStyled;
begin
  ListBox1.DefaultItemStyles.ItemStyle := 'CustomItem';
  ListBox1.ItemHeight                  := 64;
end;
 
procedure TfrmMain.Button4Click(Sender: TObject);
begin
  SetNoStyled;
  TBinder.BindDataSetToListBox(cdsSource, ListBox1, 'Common_Name');
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  SetNoStyled;
  TBinder.BindDataSetToListBox(cdsSource, ListBox1, 'Common_Name', False);
end;

procedure TfrmMain.Button6Click(Sender: TObject);
begin
  SetNoStyled;
  TBinder.BindDataSetToListBox(cdsSource, ListBox1, 'Common_Name', True, '%s + '' - '' + DataSet.Category.Text');
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
  SetNoStyled;
  TBinder.BindDataSetToListBox(cdsSource, ListBox1, 'Common_Name', False, '%s + '' - '' + DataSet.Category.Text');
end;

procedure TfrmMain.Button8Click(Sender: TObject);
begin
  SetStyled;
  TBinder.BindDataSetToListBox(cdsSource, ListBox1,
                               [
                                 TBinder.TConvData.Create('Common_Name', 'Text'),
                                 TBinder.TConvData.Create('Category', TBinder.StyledFieldOfComponent('resolution')),
                                 TBinder.TConvData.Create('Species No', TBinder.StyledFieldOfComponent('depth')),
                                 TBinder.TConvData.Create('Graphic', 'Bitmap')
                               ]);
end;

procedure TfrmMain.Button9Click(Sender: TObject);
begin
  SetStyled;
  TBinder.BindDataSetToListBox(cdsSource, ListBox1,
                               [
                                 TBinder.TConvData.Create('Common_Name', 'Text', '%s + '' - '' + DataSet.Category.Text'),
                                 TBinder.TConvData.Create('Category', TBinder.StyledFieldOfComponent('resolution')),
                                 TBinder.TConvData.Create('Species No', TBinder.StyledFieldOfComponent('depth')),
                                 TBinder.TConvData.Create('Graphic', 'Bitmap')
                               ]);
end;

procedure TfrmMain.CheckBox1Change(Sender: TObject);
begin
  ListBox1.AllowDrag := CheckBox1.IsChecked;
end;

{ TBinder }

class procedure TBinder.BindDataSetToListBox(ADataSet: TDataSet; AListBox: TListbox; const AField: String; const AOnlyFillValues: Boolean; const ACustomDisplayExpression: String);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
  I      : Integer;
begin
  for I := 0 to AListBox.ComponentCount - 1 do
  begin
    if (AListBox.Components[I] is TBindSourceDB) then
    begin
      AListBox.Components[I].Free;
      Break;
    end;
  end;

  LSource          := TBindSourceDB.Create(AListBox);
  LSource.DataSet  := ADataSet;
  if AOnlyFillValues then
  begin
    LFiller          := TLinkFillControlToField.Create(LSource);
    LFiller.Category := 'Quick Bindings';
    LFiller.Control  := AListBox;
    LFiller.Track    := True;

    LFiller.FillDataSource := LSource;
    LFiller.FillDisplayFieldName := AField;
    if not ACustomDisplayExpression.IsEmpty then
      LFiller.FillDisplayCustomFormat := ACustomDisplayExpression;
    LFiller.AutoFill := True;
    LFiller.Active   := True;
  end
  else begin
         LLinker          := TLinkListControlToField.Create(LSource);
         LLinker.Category := 'Quick Bindings';
         LLinker.Control  := AListBox;

         LLinker.DataSource := LSource;
         LLinker.FieldName  := AField;
         if not ACustomDisplayExpression.IsEmpty then
           LLinker.CustomFormat := ACustomDisplayExpression;
         LLinker.Active   := True;
       end;
end;

class procedure TBinder.BindDataSetToListBox(ADataSet: TDataSet; AListBox: TListbox; const ALinks: array of TConvData; const AOnlyFillValues: Boolean);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
  I      : Integer;
  LItem  : TCollectionItem;
  LItem1: TFormatExpressionItem;
begin
  for I := 0 to AListBox.ComponentCount - 1 do
  begin
    if (AListBox.Components[I] is TBindSourceDB) then
    begin
      AListBox.Components[I].Free;
      Break;
    end;
  end;

  LSource          := TBindSourceDB.Create(AListBox);
  LSource.DataSet  := ADataSet;
  if AOnlyFillValues then
  begin
    LFiller          := TLinkFillControlToField.Create(LSource);
    LFiller.Category := 'Quick Bindings';
    LFiller.Control  := AListBox;
    LFiller.Track    := True;

    LFiller.FillDataSource := LSource;

    for I := Low(ALinks) to High(ALinks) do
    begin
      LItem1 := LFiller.FillExpressions.AddExpression;
      LItem1.SourceMemberName  := ALinks[I].DSField;
      LItem1.ControlMemberName := ALinks[I].LBField;
      if not ALinks[I].CustomFormat.IsEmpty then
        LItem1.CustomFormat := ALinks[I].CustomFormat;
    end;

    LFiller.AutoFill := True;
    LFiller.Active   := True;
  end
  else begin
         LLinker          := TLinkListControlToField.Create(LSource);
         LLinker.Category := 'Quick Bindings';
         LLinker.Control  := AListBox;

         LLinker.DataSource := LSource;
         //LLinker.FieldName  := AField;

         for I := Low(ALinks) to High(ALinks) do
         begin
           LItem1 := LLinker.FillExpressions.AddExpression;
           LItem1.SourceMemberName  := ALinks[I].DSField;
           LItem1.ControlMemberName := ALinks[I].LBField;
           if not ALinks[I].CustomFormat.IsEmpty then
             LItem1.CustomFormat := ALinks[I].CustomFormat;
         end;

         LLinker.Active   := True;
       end;
end;

class function TBinder.StyledFieldOfComponent(const AField: String): String;
begin
  Result := 'StylesData[''' + AField + ''']';
end;

{ TBinder.TConvData }

constructor TBinder.TConvData.Create(const ADSField: String; const ALBField: String; const ACustomFormat: String);
begin
  DSField      := ADSField;
  LBField      := ALBField;
  CustomFormat := ACustomFormat;
end;

end.
