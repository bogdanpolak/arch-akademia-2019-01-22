unit Helper.TJSONObject;

interface

uses
  System.JSON;

type
  TJSONObjectHelper = class helper for TJSONObject
    function FieldAvaliable(const AFieldName: string): Boolean; inline;
    function IsValidIsoDateUTC(const AField: string; var AUtcDate: TDateTime): Boolean;
  end;

implementation

uses
  System.DateUtils, System.SysUtils;

{ TJSONObjectHelper }

function TJSONObjectHelper.fieldAvaliable(const AFieldName: string): Boolean;
begin
  Result := Assigned(Self.Values[AFieldName]) and
    not Self.Values[AFieldName].Null;
end;

function TJSONObjectHelper.IsValidIsoDateUtc(const AField: string;
  var AUtcDate: TDateTime): Boolean;
begin
  AUtcDate := 0;
  try
    AUtcDate := System.DateUtils.ISO8601ToDate(Self.Values[AField].Value, False);
    Result := True;
  except
    on E: Exception do
      Result := False;
  end
end;

end.
