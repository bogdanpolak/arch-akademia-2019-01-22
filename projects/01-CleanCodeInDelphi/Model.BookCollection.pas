unit Model.BookCollection;

interface

uses
  System.Generics.Collections,
  Model.Book,
  DataAccess.Books;

Type
  TBookCollection = class(TObjectList<TBook>)
  public
    procedure LoadDataSet(BooksDAO: IBooksDAO);
    function FindByISBN(const isbn: string): TBook;
  end;

implementation

procedure TBookCollection.LoadDataSet(BooksDAO: IBooksDAO);
begin
  BooksDAO.ForEach(
    procedure(Books: IBooksDAO)
    begin
      self.Add(TBook.Create(Books));
    end);
end;

function TBookCollection.FindByISBN(const isbn: string): TBook;
var
  book: TBook;
begin
  for book in self do
    if book.isbn = isbn then
    begin
      Result := book;
      exit;
    end;
  Result := nil;
end;

end.
