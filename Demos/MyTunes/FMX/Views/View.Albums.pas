unit View.Albums;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Actions,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.MultiView,
  FMX.ListView,
  FMX.Edit,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Layouts,
  FMX.ActnList,
  FMX.DialogService,

  MVVM.Core,
  MVVM.Controls.Platform.FMX, // MUST be listed AFTER all other FMX.* units!
  MVVM.Views.Platform.FMX,

  //Grijjy.Mvvm.Controls.Fmx,
  //Grijjy.Mvvm.Views.Fmx,

  MyTunes.Interfaces,
  ViewModel.Albums;

type
  TViewAlbums = class(TFormView<IViewModelAlbums>)
    ActionList: TActionList;
    ActionAddAlbum: TAction;
    ActionDeleteAlbum: TAction;
    ActionEditAlbum: TAction;
    LayoutMain: TLayout;
    ToolBar: TToolBar;
    SpeedButtonMaster: TSpeedButton;
    LabelAlbumDetails: TLabel;
    ButtonEditAlbum: TButton;
    RectangleBackground: TRectangle;
    MultiView: TMultiView;
    ToolBarMaster: TToolBar;
    LabelAlbums: TLabel;
    SpeedButtonAddAlbum: TSpeedButton;
    SpeedButtonDeleteAlbum: TSpeedButton;
    ListViewAlbums: TListView;
    ImageAlbumCover: TImage;
    TextTitle: TText;
    TextArtist: TText;
    ListViewTracks: TListView;
    procedure ListViewAlbumsChange(Sender: TObject);
    procedure ListViewAlbumsDeleteItem(Sender: TObject; AIndex: Integer);
  private
    { Private declarations }
    procedure DeleteAlbum;
  protected
    { TgoFormView }
    procedure SetupView; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  ViewAlbums: TViewAlbums = nil;

implementation

uses
  System.Generics.Collections,

  //Grijjy.Mvvm.DataBinding,
  MVVM.Types,

  Template.Album,
  Template.Track,
  Model.Album,
  Model.Track,
  Model;

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}
{$R *.NmXhdpiPh.fmx ANDROID}

{ TViewAlbums }

constructor TViewAlbums.Create(AOwner: TComponent);
begin
  inherited;
  ReportMemoryLeaksOnShutdown := True;

  { Always show master view, also on mobile. }
  MultiView.ShowMaster;

  InitView(TViewModelAlbums.Create);
end;

procedure TViewAlbums.DeleteAlbum;
begin
  Assert(Assigned(TViewModelAlbums(ViewModel).SelectedAlbum));
  TDialogService.MessageDialog(
    Format('Are you sure you want to delete album "%s"?', [TViewModelAlbums(ViewModel).SelectedAlbum.Title]),
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
    TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      if (AResult = mrYes) then
        TViewModelAlbums(ViewModel).DeleteAlbum;
    end);
end;

procedure TViewAlbums.ListViewAlbumsChange(Sender: TObject);
begin
  Binder.BindCollection<TAlbumTrack>(TViewModelAlbums(ViewModel).SelectedAlbum.Tracks, ListViewTracks, TTemplateTrack);
end;

procedure TViewAlbums.ListViewAlbumsDeleteItem(Sender: TObject;
  AIndex: Integer);
begin
  Binder.BindCollection<TAlbumTrack>(nil, ListViewTracks, TTemplateTrack);
end;

procedure TViewAlbums.SetupView;
begin
  inherited;

  { Bind properties }
  Binder.Bind(ViewModel.GetAsObject, 'SelectedAlbum', ListViewAlbums, 'SelectedItem');
  Binder.Bind(ViewModel.GetAsObject, 'SelectedAlbum.BackgroundColor', RectangleBackground.Fill, 'Color', EBindDirection.OneWay);
  Binder.Bind(ViewModel.GetAsObject, 'SelectedAlbum.Title', TextTitle, 'Text', EBindDirection.OneWay);
  Binder.Bind(ViewModel.GetAsObject, 'SelectedAlbum.TextColor1', TextTitle.TextSettings, 'FontColor', EBindDirection.OneWay);
  Binder.Bind(ViewModel.GetAsObject, 'SelectedAlbum.Artist', TextArtist, 'Text', EBindDirection.OneWay);
  Binder.Bind(ViewModel.GetAsObject, 'SelectedAlbum.TextColor2', TextArtist.TextSettings, 'FontColor', EBindDirection.OneWay);
  Binder.Bind(ViewModel.GetAsObject, 'SelectedAlbum.Bitmap', ImageAlbumCover, 'Bitmap', EBindDirection.OneWay);

  { Bind collections }
  Binder.BindCollection<TAlbum>(TViewModelAlbums(ViewModel).Albums, ListViewAlbums, TTemplateAlbum);

  { Bind actions }
  ActionAddAlbum.Bind(TViewModelAlbums(ViewModel).AddAlbum);
  ActionDeleteAlbum.Bind(Self.DeleteAlbum, TViewModelAlbums(ViewModel).HasSelectedAlbum);
  ActionEditAlbum.Bind(TViewModelAlbums(ViewModel).EditAlbum, TViewModelAlbums(ViewModel).HasSelectedAlbum);
end;

end.

