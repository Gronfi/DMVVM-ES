unit MyTunes.Interfaces;

interface

uses
  MVVM.Interfaces,
  Data;

type
  IMyModel = interface(IModel)
  ['{F4DF3FEE-0EE9-4261-9ABA-03D699AD5674}']
  end;

  IAlbum = interface(IModel)
  ['{FC26BD84-D8D1-4267-BD7E-0BCF584D9F6B}']
  end;

  IAlbumTrack = interface(IModel)
  ['{1D48E981-785F-418C-8938-E65CF7D61CC0}']
  end;

  IAlbumTracks = interface(IModel)
  ['{46C50CEB-1526-4B95-8C51-BAF91D803EEE}']
  end;

  IViewModelAlbum = interface(IViewModel)
  ['{CB534E0B-5901-4DCD-B5A0-057B43E32926}']
  end;

  IViewModelAlbums = interface(IViewModel)
  ['{7E2EC79E-22B9-47CE-9C00-15C3F820C25F}']
  end;

  IViewModelTracks = interface(IViewModel)
  ['{E5A2F347-DE76-407A-B17B-6BA0961B23E9}']
  end;


implementation

end.
