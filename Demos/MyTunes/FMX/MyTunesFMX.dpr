program MyTunesFMX;

uses
  System.StartUpCopy,
  FMX.Forms,

  MVVM.Binding.LiveBindings.Controls.FMX,
  MVVM.Core,

  Data in '..\Shared\Data\Data.pas',
  Model.Album in '..\Shared\Models\Model.Album.pas',
  Model in '..\Shared\Models\Model.pas',
  Model.Track in '..\Shared\Models\Model.Track.pas',
  Template.Album in '..\Shared\Templates\Template.Album.pas',
  View.Albums in 'Views\View.Albums.pas' {ViewAlbums},
  ViewModel.Albums in '..\Shared\ViewModels\ViewModel.Albums.pas',
  Template.Track in '..\Shared\Templates\Template.Track.pas',
  View.Tracks in 'Views\View.Tracks.pas' {ViewTracks},
  ViewModel.Tracks in '..\Shared\ViewModels\ViewModel.Tracks.pas',
  View.Album in 'Views\View.Album.pas' {ViewAlbum},
  ViewModel.Album in '..\Shared\ViewModels\ViewModel.Album.pas',
  Converter.TitleToCaption in '..\Shared\Converters\Converter.TitleToCaption.pas',
  MyTunes.Interfaces in '..\Shared\Interfaces\MyTunes.Interfaces.pas',
  MyTunes.Types in '..\Shared\Types\MyTunes.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  MVVMCore.DefaultBindingStrategy := 'LIVEBINDINGS';
  Application.CreateForm(TViewAlbums, ViewAlbums);
  Application.Run;
end.
