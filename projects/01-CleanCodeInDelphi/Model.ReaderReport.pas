unit Model.ReaderReport;

interface

uses
  System.JSON;

type
  TReaderReport = class
  private
    FEmail: String;
    FFirstName: String;
    FLastName: String;
    FCompany: String;
    FBookISBN: String;
    FBookTitle: String;
    FRating: Integer;
    FOppinion: String;
    FReportedDate: TDateTime;
  public
    procedure Validate;
    procedure LoadFromJSON(AJSONReport: TJSONObject);
    // --------
    property email: String read FEmail write FEmail;
    property firstName: String read FFirstName write FFirstName;
    property lastName: String read FLastName write FLastName;
    property company: String read FCompany write FCompany;
    property bookISBN: String read FBookISBN write FBookISBN;
    property bookTitle: String read FBookTitle write FBookTitle;
    property rating: Integer read FRating write FRating;
    property oppinion: String read FOppinion write FOppinion;
    property ReportedDate: TDateTime read FReportedDate write FReportedDate;
  end;

implementation

uses
  System.SysUtils,
  Helper.TJSONObject, Utils.General;

procedure TReaderReport.LoadFromJSON(AJSONReport: TJSONObject);
begin
  if AJSONReport.IsValidIsoDateUtc('created') then
    ReportedDate := AJSONReport.GetPairValueAsUtcDate('created')
  else
    raise Exception.Create('Invalid date. Expected ISO format');

  if not AJSONReport.IsValidateEmail('email') then
    raise Exception.Create('Invalid e-mail addres');

  Email := AJSONReport.Values['email'].Value;
  FirstName := AJSONReport.GetPairValueAsString('firstname');
  LastName := AJSONReport.GetPairValueAsString('lastname');
  Company := AJSONReport.GetPairValueAsString('company');
  BookISBN := AJSONReport.GetPairValueAsString('book-isbn');
  BookTitle := AJSONReport.GetPairValueAsString('book-title');
  Rating := AJSONReport.GetPairValueAsInteger('rating');
  Oppinion := AJSONReport.GetPairValueAsString('oppinion');

  Validate;
end;

procedure TReaderReport.Validate;
begin
  //Regu�y walidacji TReaderReport
  if not TValidateLibrary.CheckEmail(Email) then
    raise Exception.Create('Invalid email addres!');
end;

end.
