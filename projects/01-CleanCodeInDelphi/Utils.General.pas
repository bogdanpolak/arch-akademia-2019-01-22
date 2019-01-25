unit Utils.General;

interface

uses
  Vcl.Forms;

type
  TFrameClass = class of Vcl.Forms.TFrame;
  TNotifyLogInfo = procedure(Level: Integer; const Msg: string; Show: Boolean) of object;

  TValidateLibrary = class
    class function CheckEmail(const Value: string): Boolean;
    class function CheckIBAN(const Value: string): Boolean;
    class function IsNotEmpty(const Value: string): Boolean;
  end;

implementation

uses
  System.RegularExpressions;

class function TValidateLibrary.CheckEmail(const Value: string): Boolean;
const
  EMAIL_REGEX = '^((?>[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+\x20*|"((?=[\x01-\x7f])' +
    '[^"\\]|\\[\x01-\x7f])*"\x20*)*(?<angle><))?((?!\.)' +
    '(?>\.?[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+)+|"((?=[\x01-\x7f])' +
    '[^"\\]|\\[\x01-\x7f])*")@(((?!-)[a-zA-Z\d\-]+(?<!-)\.)+[a-zA-Z]' +
    '{2,}|\[(((?(?<!\[)\.)(25[0-5]|2[0-4]\d|[01]?\d?\d))' +
    '{4}|[a-zA-Z\d\-]*[a-zA-Z\d]:((?=[\x01-\x7f])[^\\\[\]]|\\' +
    '[\x01-\x7f])+)\])(?(angle)>)$';
begin
  Result := System.RegularExpressions.TRegEx.IsMatch(Value, EMAIL_REGEX);
end;

class function TValidateLibrary.CheckIBAN(const Value: string): Boolean;
begin
  { TODO: Dopisaæ weryfikacje IBAN }
  Result := Value <> '';
end;

class function TValidateLibrary.IsNotEmpty(const Value: string): Boolean;
begin
  Result := Value <> '';
end;

end.
