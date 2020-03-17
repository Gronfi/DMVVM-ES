unit ViewModel.Album;

interface

uses
  MVVM.Observable,
  MyTunes.Interfaces,
  //Grijjy.Mvvm.Observable,
  Model.Album;

type
  TViewModelAlbum = class(TObservable, IViewModelAlbum)
  {$REGION 'Internal Declarations'}
  private
    FAlbum: TAlbum;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AAlbum: TAlbum);

    { Actions }
    procedure EditTracks;

    procedure SetupViewModel;

    function GetAsObject: TObject;

    { Bindable properties }
    property Album: TAlbum read FAlbum;
  end;

implementation

uses
  System.UITypes,
  //Grijjy.Mvvm.Types,
  MVVM.Interfaces,
  MVVM.ViewFactory,
  Model.Track,
  ViewModel.Tracks;

{ TViewModelAlbum }

constructor TViewModelAlbum.Create(const AAlbum: TAlbum);
begin
  Assert(Assigned(AAlbum));
  inherited Create;
  FAlbum := AAlbum;
end;

procedure TViewModelAlbum.EditTracks;
var
  Clone: TAlbumTracks;
  ViewModel: IViewModelTracks;
  View: IView<IViewModelTracks>;
begin
  Clone := TAlbumTracks.Create;
  try
    Clone.Assign(Album.Tracks);
    ViewModel := TViewModelTracks.Create(Clone);

    { The view becomes owner of the view model }
    View := TViewFactory.CreateView('Tracks', nil, ViewModel);
    (View as IViewForm<IViewModelTracks>).ExecuteModal(
      procedure (AModalResult: TModalResult)
      begin
        if (AModalResult = mrOk) then
          Album.SetTracks(Clone);
        Clone.DisposeOf;
      end);
  except
    Clone.DisposeOf;
    raise;
  end;
end;

function TViewModelAlbum.GetAsObject: TObject;
begin
  Result := Self
end;

procedure TViewModelAlbum.SetupViewModel;
begin
  //
end;

end.
