unit Converter.TitleToCaption;

interface

uses
  System.RTTI,

  Mvvm.Types;

type
  { Prefix an album title with the text 'Album: ' }
  TTitleToCaption = class(TBindingValueConverter)
  public
    class function ConvertSourceToTarget(const ASource: TValue): TValue; override;
  end;

implementation

uses
  System.SysConst,
  System.SysUtils;

{ TTitleToCaption }

class function TTitleToCaption.ConvertSourceToTarget(const ASource: TValue): TValue;
begin
  Result := 'Album: ' + ASource.AsString;
end;

end.
