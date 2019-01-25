unit Action.ImportFromWebService;

interface

uses
  System.JSON,
  System.Variants,
  System.Classes,
  System.SysUtils,
  ExtGUI.ListBox.Books,
  Data.Main,
  Model.Book;

type
  TReaderReport = record
    Email: string;
    FirstName: string;
    LastName: string;
    Company: string;
    BookISBN: string;
    BookTitle: string;
    Rating: Integer;
    Oppinion: string;
    ReportedDate: TDateTime;
    procedure Validate;
    procedure LoadFromJSON(AJSONReport: TJSONObject);
  end;

  TImportFromWebService = class (TComponent)
  private
    FBooksConfig: TBooksListBoxConfigurator;
    FDataModMain: TDataModMain;
    FOnAfterExecute: TProc;
    FTagString: string;
    procedure SetBooksConfig(const Value: TBooksListBoxConfigurator);
    procedure SetDataModMain(const Value: TDataModMain);
    procedure SetOnAfterExecute(const Value: TProc);
    procedure SetTagString(const Value: string);
    procedure ImportBooks;
    procedure ImportReaderReports;
    function CreateBookFromJSON(AJSONBook: TJSONObject): TBook;

  public
    property BooksConfig: TBooksListBoxConfigurator read FBooksConfig write SetBooksConfig;
    property DataModMain: TDataModMain read FDataModMain write SetDataModMain;
    property OnAfterExecute: TProc read FOnAfterExecute write SetOnAfterExecute;
    property TagString: String read FTagString write SetTagString;
    procedure Execute;
  end;

function BooksToDateTime(const s: string): TDateTime;

implementation

uses
  Vcl.Forms,
  Vcl.DBGrids,
  Data.DB,
  Frame.Import,
  ClientAPI.Books, ClientAPI.Readers,
  Helper.TJSONObject, Helper.DataSet, Helper.TApplication, Helper.Variant,
  Utils.General;

function BooksToDateTime(const s: string): TDateTime;
const
  months: array [1 .. 12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  m: string;
  y: string;
  i: Integer;
  mm: Integer;
  yy: Integer;
begin
  m := s.Substring(0, 3);
  y := s.Substring(4);
  mm := 0;
  for i := 1 to 12 do
    if months[i].ToUpper = m.ToUpper then
      mm := i;
  if mm = 0 then
    raise ERangeError.Create('Incorect mont name in the date: ' + s);
  yy := y.ToInteger();
  Result := EncodeDate(yy, mm, 1);
end;

const
  Client_API_Token = '20be805d-9cea27e2-a588efc5-1fceb84d-9fb4b67c';


{ TImportFormWebService }

function TImportFromWebService.CreateBookFromJSON(AJSONBook: TJSONObject): TBook;
begin
  Result := TBook.Create;
  Result.Status := AJSONBook.Values['status'].Value;
  Result.Title := AJSONBook.Values['title'].Value;
  Result.Isbn := AJSONBook.Values['isbn'].Value;
  Result.Author := AJSONBook.Values['author'].Value;
  Result.ReleseDate := BooksToDateTime(AJSONBook.Values['date'].Value);
  Result.Pages := (AJSONBook.Values['pages'] as TJSONNumber).AsInt;
  Result.Price := StrToCurr(AJSONBook.Values['price'].Value);
  Result.Currency := AJSONBook.Values['currency'].Value;
  Result.Description := AJSONBook.Values['description'].Value;
  Result.Imported := Now();
  Result.Validate;
end;


procedure TImportFromWebService.Execute;
begin
  ImportBooks;
  ImportReaderReports;
end;

procedure TImportFromWebService.ImportBooks;
var
  I: Integer;
  LBook, LFindBook: TBook;
  jsBooks: TJSONArray;
begin
  // Import new Books data from OpenAPI
  jsBooks := ImportBooksFromWebService(Client_API_Token);
  try
    for i := 0 to jsBooks.Count - 1 do
    begin
      LBook := CreateBookFromJSON(jsBooks.Items[i] as TJSONObject);
      LFindBook := FBooksConfig.GetBookList(blkAll).FindByISBN(LBook.isbn);

      if not Assigned(LFindBook) then
      begin
        FBooksConfig.InsertNewBook(LBook);
        // ----------------------------------------------------------------
        // Append report into the database:
        // Fields: ISBN, Title, Authors, Status, ReleseDate, Pages, Price,
        // Currency, Imported, Description
        DataModMain.dsBooks.InsertRecord([LBook.Isbn, LBook.Title, LBook.Author,
          LBook.Status, LBook.ReleseDate, LBook.Pages, LBook.Price,
          LBook.Currency, LBook.Imported, LBook.Description]);
      end
      else
        LBook.Free;
    end;
  finally
    jsBooks.Free;
  end;
end;

procedure TImportFromWebService.ImportReaderReports;
var
  jsData: TJSONArray;
  I: Integer;
  lReaderReport: TReaderReport;
  lBook: TBook;
  lRatings: array of string;
  lReaderId: Variant;
begin
  // Import new Reader Reports data from OpenAPI
  // - Load JSON from WebService
  // - Validate JSON and insert new a Readers into the Database
  jsData := ImportReaderReportsFromWebService(Client_API_Token);
  try
    for i := 0 to jsData.Count - 1 do
    begin
      // Read JSON object
      lReaderReport.LoadFromJSON(jsData.Items[i] as TJSONObject);

      // Locate book by ISBN
      lBook := FBooksConfig.GetBookList(blkAll).FindByISBN(lReaderReport.BookISBN);
      if not Assigned(lBook) then
        raise Exception.Create('Invalid book isbn');
      // ----------------------------------------------------------------
      // Find the Reader in then database using an email address
      lReaderId := DataModMain.FindReaderByEmil(lReaderReport.Email);
      // ----------------------------------------------------------------
      //
      // Append a new reader into the database if requred:
      if lReaderId.IsNull then
      begin
        lReaderId := DataModMain.dsReaders.GetMaxValue('ReaderId') + 1;

        // Fields: ReaderId, FirstName, LastName, Email, Company, BooksRead,
        // LastReport, ReadersCreated
        DataModMain.dsReaders.AppendRecord([lReaderId, lReaderReport.FirstName,
          lReaderReport.LastName, lReaderReport.Email, lReaderReport.Company, 1,
          lReaderReport.ReportedDate, Now()]);
      end;

      // Append report into the database:
      // Fields: ReaderId, ISBN, Rating, Oppinion, Reported
      DataModMain.dsReports.AppendRecord([lReaderId, lReaderReport.BookISBN,
        lReaderReport.Rating, lReaderReport.Oppinion,
        lReaderReport.ReportedDate]);
      // ----------------------------------------------------------------
      if Application.IsDeveloperMode then
        Insert([lReaderReport.Rating.ToString], lRatings, maxInt);
    end;
    // ----------------------------------------------------------------
    if Application.IsDeveloperMode then
    begin
      TagString := string.Join(' ,', lRatings);
      if Assigned(FOnAfterExecute) then
        FOnAfterExecute();
    end;
  finally
    jsData.Free;
  end;
end;

procedure TImportFromWebService.SetBooksConfig(const Value: TBooksListBoxConfigurator);
begin
  FBooksConfig := Value;
end;

procedure TImportFromWebService.SetDataModMain(const Value: TDataModMain);
begin
  FDataModMain := Value;
end;

procedure TImportFromWebService.SetOnAfterExecute(const Value: TProc);
begin
  FOnAfterExecute := Value;
end;

procedure TImportFromWebService.SetTagString(const Value: String);
begin
  FTagString := Value;
end;


{ TReaderReport }

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
  //Regu³y walidacji TReaderReport
  if not TValidateLibrary.CheckEmail(Email) then
    raise Exception.Create('Invalid email addres!');
end;

end.
