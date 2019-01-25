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
  TImportFromWebService = class(TComponent)
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
    procedure AddNewBookToDataModuleFromWeb(jsBooks: TJSONArray);
  public
    property BooksConfig: TBooksListBoxConfigurator read FBooksConfig
      write SetBooksConfig;
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
  Utils.General, Model.ReaderReport;

function BooksToDateTime(const s: string): TDateTime;
const
  months: array [1 .. 12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  m: string;
  Y: string;
  i: Integer;
  mm: Integer;
  yy: Integer;
begin
  m := s.Substring(0, 3);
  Y := s.Substring(4);
  mm := 0;
  for i := 1 to 12 do
    if months[i].ToUpper = m.ToUpper then
      mm := i;
  if mm = 0 then
    raise ERangeError.Create('Incorect mont name in the date: ' + s);
  yy := Y.ToInteger();
  Result := EncodeDate(yy, mm, 1);
end;

const
  Client_API_Token = '20be805d-9cea27e2-a588efc5-1fceb84d-9fb4b67c';

  { TImportFormWebService }


procedure TImportFromWebService.Execute;
begin
  ImportBooks;
  ImportReaderReports;
end;

procedure TImportFromWebService.ImportBooks;
var
  jsBooks: TJSONArray;
begin
  jsBooks := ImportBooksFromWebService(Client_API_Token);
  try
    AddNewBookToDataModuleFromWeb(jsBooks);
  finally
    jsBooks.Free;
  end;
end;

procedure TImportFromWebService.AddNewBookToDataModuleFromWeb
  (jsBooks: TJSONArray);
var
  jsBook: TJSONObject;
  b: TBook;
  b2: TBook;
  i: Integer;
begin
  for i := 0 to jsBooks.Count - 1 do
  begin
    jsBook := jsBooks.Items[i] as TJSONObject;
    b := TBook.Create();
    // TODO: Dodaæ try-finnaly oraz zwalanie b.Free;
    // czyli b przekazywane do InsertNewBook musi byæ sklonowane
    b.LoadFromJSON (jsBook);
    b.Validate;
    b2 := FBooksConfig.GetBookList(blkAll).FindByISBN(b.isbn);
    if not Assigned(b2) then
    begin
      FBooksConfig.InsertNewBook(b);
      // ----------------------------------------------------------------
      // Append report into the database:
      // Fields: ISBN, Title, Authors, Status, ReleseDate, Pages, Price,
      // Currency, Imported, Description
      DataModMain.dsBooks.InsertRecord([b.isbn, b.title, b.author, b.status,
        b.releseDate, b.pages, b.price, b.currency, b.imported, b.description]);
    end
    else
      b.Free;
  end;
end;


procedure TImportFromWebService.ImportReaderReports;
var
  jsData: TJSONArray;
  I: Integer;
  ReaderReport: TReaderReport;
  Book: TBook;
  Ratings: array of string;
  ReaderId: Variant;
begin
  jsData := ImportReaderReportsFromWebService(Client_API_Token);
  try
    for i := 0 to jsData.Count - 1 do
    begin
      // ----------------------------------------------------
      // Validate ReadeReport (JSON) and insert into the Database
      // Read JSON object
      ReaderReport := TReaderReport.Create;
      try
      ReaderReport.LoadFromJSON(jsData.Items[i] as TJSONObject);
      Book := FBooksConfig.GetBookList(blkAll).FindByISBN(ReaderReport.BookISBN);
      if Assigned(Book) then
      begin
      ReaderId := DataModMain.FindReaderByEmil(ReaderReport.Email);
      if ReaderId.IsNull then
      begin
        ReaderId := DataModMain.dsReaders.GetMaxValue('ReaderId') + 1;
        // Fields: ReaderId, FirstName, LastName, Email, Company, BooksRead,
        // LastReport, ReadersCreated
        DataModMain.dsReaders.AppendRecord([ReaderId, ReaderReport.FirstName,
          ReaderReport.LastName, ReaderReport.Email, ReaderReport.Company, 1,
          ReaderReport.ReportedDate, Now()]);
      end;
      // Append report into the database:
      // Fields: ReaderId, ISBN, Rating, Oppinion, Reported
      DataModMain.dsReports.AppendRecord([ReaderId, ReaderReport.BookISBN,
        ReaderReport.Rating, ReaderReport.Oppinion,
        ReaderReport.ReportedDate]);
      // ----------------------------------------------------------------
      if Application.IsDeveloperMode then
        Insert([ReaderReport.Rating.ToString], Ratings, maxInt);
      end
      else
        raise Exception.Create('Invalid book isbn');
    finally
      ReaderReport.Free;
    end;
    end;
    if Application.IsDeveloperMode then
    begin
      TagString := string.Join(' ,', Ratings);
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

end.
