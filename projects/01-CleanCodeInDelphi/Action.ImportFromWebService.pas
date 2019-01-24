unit Action.ImportFromWebService;

interface

uses
  System.JSON,
  System.Variants,
  System.Classes,
  System.SysUtils,
  ExtGUI.ListBox.Books,
  Data.Main;

type
  TImportFromWebService = class(TComponent)
  private
    FBooksConfig: TBooksListBoxConfigurator;
    FDataModMain: TDataModMain;
    FOnAfterExecute: TProc;
    FTagString: String;
    procedure SetBooksConfig(const Value: TBooksListBoxConfigurator);
    procedure SetDataModMain(const Value: TDataModMain);
    procedure SetOnAfterExecute(const Value: TProc);
    procedure SetTagString(const Value: String);
  public
    property BooksConfig: TBooksListBoxConfigurator read FBooksConfig
      write SetBooksConfig;
    property DataModMain: TDataModMain read FDataModMain write SetDataModMain;
    property OnAfterExecute: TProc read FOnAfterExecute write SetOnAfterExecute;
    property TagString: String read FTagString write SetTagString;
    procedure Execute;
    procedure ImportNewReaderReports();
  end;

function BooksToDateTime(const s: string): TDateTime;

implementation

uses
  System.Generics.Collections,
  Vcl.Forms,
  Vcl.DBGrids,
  Data.DB,
  Frame.Import,
  ClientAPI.Books, ClientAPI.Readers, Helper.TJSONObject, Helper.DataSet,
  Helper.TApplication, Utils.General;

type
  TrecBook = record
    email, firstName, lastName, company, bookISBN, bookTitle, oppinion: String;
    rating: Integer;
    dtReported: TDateTime;
  end;

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

// const
// Client_API_Token = '20be805d-9cea27e2-a588efc5-1fceb84d-9fb4b67c';

{ TImportFormWebService }

procedure TImportFromWebService.Execute;
var
  i: Integer;
  b: TBook;
  jsBooks: TJSONArray;
  jsBook: TJSONObject;
  TextBookReleseDate: string;
  b2: TBook;
begin
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // Import new Books data from OpenAPI
  //

  { TODO 2: [A] Extract method. Read comments and use meaningful name }
  jsBooks := ImportBooksFromWebService(Client_API_Token);
  try
    for i := 0 to jsBooks.Count - 1 do
    begin
      jsBook := jsBooks.Items[i] as TJSONObject;
      b := TBook.Create;
      b.status := jsBook.Values['status'].Value;
      b.title := jsBook.Values['title'].Value;
      b.isbn := jsBook.Values['isbn'].Value;
      b.author := jsBook.Values['author'].Value;
      TextBookReleseDate := jsBook.Values['date'].Value;
      b.releseDate := BooksToDateTime(TextBookReleseDate);
      b.pages := (jsBook.Values['pages'] as TJSONNumber).AsInt;
      b.price := StrToCurr(jsBook.Values['price'].Value);
      b.currency := jsBook.Values['currency'].Value;
      b.description := jsBook.Values['description'].Value;
      b.imported := Now();
      b2 := FBooksConfig.GetBookList(blkAll).FindByISBN(b.isbn);
      if not Assigned(b2) then
      begin
        FBooksConfig.InsertNewBook(b);
        // ----------------------------------------------------------------
        // Append report into the database:
        // Fields: ISBN, Title, Authors, Status, ReleseDate, Pages, Price,
        // Currency, Imported, Description
        DataModMain.dsBooks.InsertRecord([b.isbn, b.title, b.author, b.status,
          b.releseDate, b.pages, b.price, b.currency, b.imported,
          b.description]);
      end;
    end;
  finally
    jsBooks.Free;
  end;

  ImportNewReaderReports();
end;

procedure TImportFromWebService.SetBooksConfig(const Value
  : TBooksListBoxConfigurator);
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

function LoadFromJSON(jsRow: TJSONObject): TrecBook;
begin
  Result.firstName := jsRow.GetPairValueAsString('firstname');
  Result.lastName := jsRow.GetPairValueAsString('lastname');
  Result.company := jsRow.GetPairValueAsString('company');
  Result.bookISBN := jsRow.GetPairValueAsString('book-isbn');
  Result.bookTitle := jsRow.GetPairValueAsString('book-title');
  Result.rating := jsRow.GetPairValueAsInteger('rating');
  Result.oppinion := jsRow.GetPairValueAsString('oppinion');
  Result.email := jsRow.Values['email'].Value;
  if not CheckEmail(Result.email) then
    raise Exception.Create('Invalid email addres');

  if jsRow.IsValidIsoDateUtc('created') then
    Result.dtReported := jsRow.GetPairValueAsUtcDate('created')
  else
  begin
    Result.dtReported := 0;
    raise Exception.Create('Invalid date. Expected ISO format');
  end;
end;

procedure TImportFromWebService.ImportNewReaderReports();
var
  recBook: TrecBook;
  jsData: TJSONArray;
  i: Integer;
  ss: array of string;
  readerId: Variant;
  b: TBook;
begin
  jsData := ImportReaderReportsFromWebService(Client_API_Token);
  { TODO 2: [D] Extract method. Block try-catch is separate responsibility }
  try
    for i := 0 to jsData.Count - 1 do
    begin
      { TODO 4: [A] Extract Reader Report code into the record TReaderReport (model layer) }
      { TODO 2: [F] Repeated code. Violation of the DRY rule }
      // Use TJSONObject helper Values return Variant.Null
      // ----------------------------------------------------------------
      //
      // Read JSON object
      //
      recBook := LoadFromJSON(jsData.Items[i] as TJSONObject);

      // ----------------------------------------------------------------
      //
      // Locate book by ISBN
      //
      { TODO 2: [G] Extract method }
      b := FBooksConfig.GetBookList(blkAll).FindByISBN(recBook.bookISBN);
      if not Assigned(b) then
        raise Exception.Create('Invalid book isbn');
      // ----------------------------------------------------------------
      // Find the Reader in then database using an email address
      readerId := DataModMain.FindReaderByEmil(recBook.email);
      // ----------------------------------------------------------------
      //
      // Append a new reader into the database if requred:
      if VarIsNull(readerId) then
      begin
        { TODO 2: [G] Extract method }
        readerId := DataModMain.dsReaders.GetMaxValue('ReaderId') + 1;
        //
        // Fields: ReaderId, FirstName, LastName, Email, Company, BooksRead,
        // LastReport, ReadersCreated
        //
        DataModMain.dsReaders.AppendRecord([readerId, recBook.firstName,
          recBook.lastName, recBook.email, recBook.company, 1,
          recBook.dtReported, Now()]);
      end;
      // ----------------------------------------------------------------
      //
      // Append report into the database:
      // Fields: ReaderId, ISBN, Rating, Oppinion, Reported
      //
      DataModMain.dsReports.AppendRecord([readerId, recBook.bookISBN,
        recBook.rating, recBook.oppinion, recBook.dtReported]);
      // ----------------------------------------------------------------
      if Application.IsDeveloperMode then
        Insert([recBook.rating.ToString], ss, maxInt);
    end;
    if Application.IsDeveloperMode then
    begin
      TagString := String.Join(' ,', ss);
      if Assigned(FOnAfterExecute) then
        FOnAfterExecute();
    end;
  finally
    jsData.Free;
  end;
end;

end.
