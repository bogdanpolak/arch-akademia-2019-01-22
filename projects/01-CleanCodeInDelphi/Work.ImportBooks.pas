unit Work.ImportBooks;

interface

uses
  System.JSON,
  Plus.TWork,
  ExtGUI.ListBox.Books;

type
  TImportBooksWork = class (TWork)
  private
    FBooksConfig: TBooksListBoxConfigurator;
    procedure DoImportBooks;
    procedure AddNewBookToDataModuleFromWeb(jsBooks: TJSONArray);
    procedure SetBooksConfig(const Value: TBooksListBoxConfigurator);
  public
    procedure Execute; override;
    property BooksConfig: TBooksListBoxConfigurator read FBooksConfig write SetBooksConfig;
  end;


implementation

{ TImportBooksWork }

uses
  ClientAPI.Books,
  Consts.Application,
  Model.Book,
  Data.Main;


procedure TImportBooksWork.AddNewBookToDataModuleFromWeb
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


procedure TImportBooksWork.DoImportBooks;
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

procedure TImportBooksWork.Execute;
begin
  inherited;
  DoImportBooks
end;

procedure TImportBooksWork.SetBooksConfig(
  const Value: TBooksListBoxConfigurator);
begin
  FBooksConfig := Value;
end;

end.
