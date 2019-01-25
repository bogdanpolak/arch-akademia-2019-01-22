unit Helper.TJSONObject;

interface

uses
  System.JSON;

type
  TJSONObjectHelper = class helper for TJSONObject
    /// <summary>
    /// Funkcja sprawdza czy para z kluczem "Key" jest dost�pna oraz czy
    /// jej warto�� nie jest Null (TJSONNull)
    /// </summary>
    function IsPairAvaliableAndNotNull(const Key: string): Boolean;
    /// <summary>
    /// Pobiera tekstow� (TJSONString) warto�� pary z kluczem "Key". Sprawdza
    /// czy para jest dost�pna.
    /// </summary>
    function GetPairValueAsString(const Key: string): string;
    /// <summary>
    /// Pobiera liczbow� warto�� (TJSONNumber) pary z kluczem "Key".
    /// Zwraca warto�� jako Integer. Sprawdza czy para jest dost�pna.
    /// </summary>
    function GetPairValueAsInteger(const Key: string): Integer;
    /// <summary>
    /// Pobiera wartto�� pary JSON o kluczu Key. Traktuj� j� jako tekst
    /// w formacie ISO Date (ISO8601) UTC i konwertuje j� do TDateTime
    /// </summary>
    function GetPairValueAsUtcDate(const Key: string): TDateTime;
    function IsValidIsoDateUtc(const Key: string): Boolean;
    function IsValidateEmail(const Key: string): Boolean;
  end;

implementation

uses
  System.DateUtils,
  Utils.General;

function TJSONObjectHelper.IsPairAvaliableAndNotNull(const Key: string): Boolean;
begin
  Result := Assigned(Self.Values[Key]) and not Self.Values[Key].Null;
end;

function TJSONObjectHelper.IsValidateEmail(const Key: string): Boolean;
begin
  Result := IsPairAvaliableAndNotNull(Key) and
    TValidateLibrary.CheckEmail(Self.Values[Key].Value);
end;

function TJSONObjectHelper.GetPairValueAsInteger(const Key: string): integer;
begin
  if IsPairAvaliableAndNotNull(Key) then
    Result := (Self.Values[Key] as TJSONNumber).AsInt
  else
    Result := -1;
end;

function TJSONObjectHelper.GetPairValueAsString(const Key: string): String;
begin
  if IsPairAvaliableAndNotNull(Key) then
    Result := Self.Values[Key].Value
  else
    Result := '';
end;

function TJSONObjectHelper.IsValidIsoDateUtc(const Key: string): Boolean;
var
  dt: TDateTime;
begin
  dt := 0;
  Result := System.DateUtils.TryISO8601ToDate(Self.Values[Key].Value, dt);
end;

function TJSONObjectHelper.GetPairValueAsUtcDate(const Key: string): TDateTime;
begin
  Result := System.DateUtils.ISO8601ToDate(Self.Values[Key].Value, False);;
end;

end.
