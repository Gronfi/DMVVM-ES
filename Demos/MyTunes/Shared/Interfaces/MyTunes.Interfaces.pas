unit MyTunes.Interfaces;

interface

uses
  System.TimeSpan,
  System.UITypes,
  System.Generics.Collections,
  FMX.Graphics,

  MVVM.Observable,
  MVVM.Interfaces,
  MVVM.Types,

  Data;

type
  IAlbum = interface;

  TAlbums = TObservableCollection<IAlbum>;

  IAlbumTrack = interface;

  IMyModel = interface(IModel)
  ['{F4DF3FEE-0EE9-4261-9ABA-03D699AD5674}']
    //constructor CreateSingleton(const ADummy: Integer = 0);
    procedure LoadAlbums(const ASource: TArray<TDataAlbum>);
    procedure LoadTracks(const ASource: TArray<TDataTrack>; const AAlbum: IAlbum);

    function GetAlbums: TAlbums;

    constructor Create;
    destructor Destroy; override;

    { Bindable properties }
    property Albums: TAlbums read GetAlbums;
  end;

  IAlbum = interface(IModel)
  ['{FC26BD84-D8D1-4267-BD7E-0BCF584D9F6B}']
    function GetTitle: string;
    procedure SetTitle(const Value: String);
    function GetTrackCount: Integer; inline;
    function GetArtist: String;
    procedure SetArtist(const Value: String);
    function GetBitmap: TObject;
    function GetCopyright: string;
    procedure SetCopyright(const Value: String);
    function GetNotes: string;
    procedure SetNotes(const Value: String);
    function GetRecordLabel: string;
    procedure SetRecordLabel(const Value: String);
    function GetReleaseDate: TDateTime;
    procedure SetReleaseDate(const Value: TDateTime);
    function GetBackgroundColor: TAlphaColor;
    procedure SetBackgroundColor(const Value: TAlphaColor);
    function GetTextColor1: TAlphaColor;
    procedure SetTextColor1(const Value: TAlphaColor);
    function GetTextColor2: TAlphaColor;
    procedure SetTextColor2(const Value: TAlphaColor);
    function GetTextColor3: TAlphaColor;
    procedure SetTextColor3(const Value: TAlphaColor);

    function GetIsValid: Boolean;
    function GetTracks: TEnumerable<IAlbumTrack>; inline;

    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: IAlbum);

    function AddTrack: IAlbumTrack;
    procedure RemoveTrack(ATrack: IAlbumTrack);
    procedure SetTracks(ATracks: IAlbumTracks);

    //property RawImage: TBytes read FRawImage write FRawImage;

    { Bindable properties }
    property Title: String read GetTitle write SetTitle;
    property Artist: String read GetArtist write SetArtist;
    property RecordLabel: String read GetRecordLabel write SetRecordLabel;
    property Copyright: String read GetCopyright write SetCopyright;
    property Notes: String read GetNotes write SetNotes;
    property ReleaseDate: TDateTime read GetReleaseDate write SetReleaseDate;
    property BackgroundColor: TAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property TextColor1: TAlphaColor read GetTextColor1 write SetTextColor1;
    property TextColor2: TAlphaColor read GetTextColor2 write SetTextColor2;
    property TextColor3: TAlphaColor read GetTextColor3 write SetTextColor3;
    property Tracks: TEnumerable<IAlbumTrack> read GetTracks;
    property TrackCount: Integer read GetTrackCount;
    property Bitmap: TObject read GetBitmap;
    property IsValid: Boolean read GetIsValid;
  end;

  IAlbumTrack = interface
    function GetDuration: TTimeSpan;
    procedure SetDuration(const Value: TTimeSpan);
    function GetName: String;
    procedure SetName(const Value: String);
    function GetGenres: String;
    procedure SetGenres(const Value: String);
    function GetTrackNumber: Integer;
    procedure SetTrackNumber(const Value: Integer);

    procedure Assign(const ASource: IAlbumTrack);

    { Bindable properties }
    property Name: String read GetName write SetName;
    property Duration: TTimeSpan read GetDuration write SetDuration;
    property TrackNumber: Integer read GetTrackNumber write SetTrackNumber;
    property Genres: String read GetGenres write SetGenres;
  end;

  IAlbumTracks = interface(IObservableCollection<IAlbumTrack>)
  ['{46C50CEB-1526-4B95-8C51-BAF91D803EEE}']
    constructor Create;
    procedure Assign(const ASource: TEnumerable<TAlbumTrack>);
  end;


implementation

end.
