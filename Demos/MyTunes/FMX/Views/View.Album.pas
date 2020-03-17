unit View.Album;

interface

uses
  System.RTTI,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ListBox,
  FMX.Layouts,
  FMX.Edit,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.DateTimeCtrls,
  FMX.Colors,

  MyTunes.Interfaces,

  MVVM.Controls.Platform.FMX,
  MVVM.Views.Platform.FMX,
  //Grijjy.Mvvm.Controls.Fmx, // MUST be listed AFTER all other FMX.* units!
  //Grijjy.Mvvm.Views.Fmx,

  ViewModel.Album;

type
  TViewAlbum = class(TFormView<IViewModelAlbum>)
    ToolBar: TToolBar;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ListBox: TListBox;
    ListBoxItemTitle: TListBoxItem;
    EditTitle: TEdit;
    ListBoxItemArtist: TListBoxItem;
    EditArtist: TEdit;
    ListBoxItemRecordLabel: TListBoxItem;
    EditRecordLabel: TEdit;
    ListBoxItemCopyright: TListBoxItem;
    EditCopyright: TEdit;
    ListBoxItemReleaseDate: TListBoxItem;
    DateEditReleaseDate: TDateEdit;
    ListBoxItemNotes: TListBoxItem;
    MemoNotes: TMemo;
    ListBoxItemTracks: TListBoxItem;
    ListBoxItemBackgroundColor: TListBoxItem;
    ComboColorBoxBackground: TComboColorBox;
    procedure ListBoxItemTracksClick(Sender: TObject);
  protected
    { TgoFormView }
    procedure SetupView; override;
  end;

implementation

uses
  System.SysConst,

  MVVM.Interfaces,
  MVVM.ViewFactory,
  MVVM.Types,
//  Grijjy.Mvvm.Rtti,
//  Grijjy.Mvvm.Types,
//  Grijjy.Mvvm.DataBinding,
//  Grijjy.Mvvm.ViewFactory,
  Converter.TitleToCaption;

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}
{$R *.Macintosh.fmx MACOS}

type
  { Prefix an album title with the text 'Album: ' }
  TTrackCountToText = class(TBindingValueConverter)
  public
    class function ConvertSourceToTarget(const ASource: TValue): TValue; override;
  end;

{ TViewAlbum }

procedure TViewAlbum.ListBoxItemTracksClick(Sender: TObject);
begin
  TViewModelAlbum(ViewModel).EditTracks;
end;

procedure TViewAlbum.SetupView;
begin
  { Bind properties }
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'Title', Self, 'Caption', EBindDirection.OneWay, [], TTitleToCaption);
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'Title', EditTitle, 'Text', EBindDirection.TwoWay, [EBindFlag.TargetTracking]);
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'Artist', EditArtist, 'Text', EBindDirection.TwoWay, [EBindFlag.TargetTracking]);
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'RecordLabel', EditRecordLabel, 'Text');
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'Copyright', EditCopyright, 'Text');
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'ReleaseDate', DateEditReleaseDate, 'Date');
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'BackgroundColor', ComboColorBoxBackground, 'Color');
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'Notes', MemoNotes, 'Text');
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'TrackCount', ListBoxItemTracks.ItemData, 'Detail', EBindDirection.OneWay, [], TTrackCountToText);
  Binder.Bind(TViewModelAlbum(ViewModel).Album, 'IsValid', ButtonOK, 'Enabled', EBindDirection.OneWay);
end;

{ TTrackCountToText }

class function TTrackCountToText.ConvertSourceToTarget(const ASource: TValue): TValue;
var
  Count: Integer;
begin
  Count := ASource.AsOrdinal;
  case Count of
    0: Result := 'No tracks';
    1: Result := '1 track';
  else
    Result := Count.ToString + ' tracks';
  end;
end;

initialization
  TViewFactory.Register(TViewAlbum, 'Album');

end.
