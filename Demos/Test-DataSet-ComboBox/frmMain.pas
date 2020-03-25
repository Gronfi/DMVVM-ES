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
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.DBScope, Data.DB,
  Datasnap.DBClient, FMX.Controls.Presentation, Data.Bind.Controls,
  System.Bindings.Helper,
  Fmx.Bind.Navigator;

type
  TfrmCustomList = class(TForm)
    Label1: TLabel;
    Image1: TImage;
    Image2: TImage;
    cdsSource: TClientDataSet;
    BindingsList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    ComboBox1: TComboBox;
    Button4: TButton;
    NavigatorBindSourceDB1: TBindNavigator;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    ListBox2: TListBox;
    LinkListControlToField2: TLinkListControlToField;
    Label2: TLabel;
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
    procedure DoInfoClick(Sender: TObject);
    procedure DoVisibleChange(Sender: TObject);
  public
    { Public declarations }
  end;

  TBinder = class
  public
    class procedure BindDataSetToCombobox(ADataSet: TDataSet; AComboBox: TCombobox; const AField: String; const AOnlyFillValues: Boolean = True; const ACustomDisplayExpression: String = ''); overload; static;
  end;

var
  frmCustomList: TfrmCustomList;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

function FindItemParent(Obj: TFmxObject; ParentClass: TClass): TFmxObject;
begin
  Result := nil;
  if Assigned(Obj.Parent) then
    if Obj.Parent.ClassType = ParentClass then
      Result := Obj.Parent
    else
      Result := FindItemParent(Obj.Parent, ParentClass);
end;


procedure TfrmCustomList.DoInfoClick(Sender: TObject);
begin
end;

procedure TfrmCustomList.DoVisibleChange(Sender: TObject);
begin
end;

procedure TfrmCustomList.Button4Click(Sender: TObject);
begin
  TBinder.BindDataSetToCombobox(cdsSource, ComboBox1, 'Common_Name');
end;

procedure TfrmCustomList.Button5Click(Sender: TObject);
begin
  TBinder.BindDataSetToCombobox(cdsSource, ComboBox1, 'Common_Name', False);
end;

procedure TfrmCustomList.Button6Click(Sender: TObject);
begin
  TBinder.BindDataSetToCombobox(cdsSource, ComboBox1, 'Common_Name', True, '%s + '' - '' + DataSet.Category.Text');
end;

procedure TfrmCustomList.Button7Click(Sender: TObject);
begin
  TBinder.BindDataSetToCombobox(cdsSource, ComboBox1, 'Common_Name', False, '%s + '' - '' + DataSet.Category.Text');
end;

procedure TfrmCustomList.CheckBox1Change(Sender: TObject);
begin

end;

{ TBinder }

// ACustomDisplayExpression: %s + ' - ' + DataSet.Category.Text
//        where ---> %s : the default text of the selected field (AField) in the dataset
//                   DataSet : references to the dataset
//                   DataSet.Category : references to the value of that fieldname in the dataset in the selected row
// AOnlyFillValues --> True: the combobox is only filled with values
//                     False: the combobox is synchronized with the dataset, if you select a row in the combobox the dataset is moved to the selected row
class procedure TBinder.BindDataSetToCombobox(ADataSet: TDataSet; AComboBox: TCombobox; const AField: String; const AOnlyFillValues: Boolean; const ACustomDisplayExpression: String);
var
  LFiller: TLinkFillControlToField;
  LLinker: TLinkListControlToField;
  LSource: TBindSourceDB;
  I      : Integer;
begin
  for I := 0 to AComboBox.ComponentCount - 1 do
  begin
    if (AComboBox.Components[I] is TBindSourceDB) then
    begin
      AComboBox.Components[I].Free;
      Break;
    end;
  end;

  LSource          := TBindSourceDB.Create(AComboBox);
  LSource.DataSet  := ADataSet;
  if AOnlyFillValues then
  begin
    LFiller          := TLinkFillControlToField.Create(LSource);
    LFiller.Category := 'Quick Bindings';
    LFiller.Control  := AComboBox;
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
         LLinker.Control  := AComboBox;

         LLinker.DataSource := LSource;
         LLinker.FieldName  := AField;
         if not ACustomDisplayExpression.IsEmpty then
           LLinker.CustomFormat := ACustomDisplayExpression;
         LLinker.Active   := True;
       end;
end;

end.
