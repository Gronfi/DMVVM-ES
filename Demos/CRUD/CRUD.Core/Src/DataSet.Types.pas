unit DataSet.Types;

interface

uses
  System.Rtti,
  Data.DB;

type
  TFieldToGet = record
    FieldName: string;
    IsBitmap: Boolean;
  end;

  TFieldsToGet = array of TFieldToGet;

  TFieldsToGetHelper = record helper for TFieldsToGet
  public
    procedure AddData(const AFieldName: string; const AIsBitmap: Boolean = False);
  end;

  TFieldConverter = record
    FieldName: string;
    FieldValue: TValue;
  end;

  TFieldConverters = array of TFieldConverter;

  TFieldConvertersHelper = record helper for TFieldConverters
  public
    procedure AddData(const AFieldName: string; AValue: TValue);
  end;

implementation

{ TFieldConvertersHelper }

procedure TFieldConvertersHelper.AddData(const AFieldName: string; AValue: TValue);
var
  LCnt: Integer;
begin
  LCnt := Length(Self);
  SetLength(Self, LCnt + 1);
  Self[LCnt].FieldName  := AFieldName;
  Self[LCnt].FieldValue := AValue;
end;

{ TFieldsToGetHelper }

procedure TFieldsToGetHelper.AddData(const AFieldName: string; const AIsBitmap: Boolean = False);
var
  LCnt: Integer;
begin
  LCnt := Length(Self);
  SetLength(Self, LCnt + 1);
  Self[LCnt].FieldName := AFieldName;
  Self[LCnt].IsBitmap  := AIsBitmap;
end;

end.
