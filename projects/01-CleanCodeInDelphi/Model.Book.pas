unit Model.Book;

interface

uses
  System.JSON,
  DataAccess.Books;

Type
  TBookListKind = (blkAll, blkOnShelf, blkAvaliable);

  TBook = class
  private
    FIsbn: string;
    procedure SetIsbn(const Value: string);
  public
    Status: string;
    Title: string;
    Author: string;
    ReleseDate: TDateTime;
    Pages: integer;
    Price: currency;
    Currency: string;
    Imported: TDateTime;
    Description: string;
    procedure Validate;
    property Isbn: string read FIsbn write SetIsbn;
    constructor Create(Books: IBooksDAO); overload;
    procedure LoadFromJSON (jsBook: TJSONObject);
  end;

implementation

{ TBook }

uses
  Utils.General,
  System.Classes,
  System.SysUtils;

constructor TBook.Create(Books: IBooksDAO);
begin
  inherited Create;
  self.isbn := Books.fldISBN.Value;
  self.title := Books.fldTitle.Value;
  self.author := Books.fldAuthors.Value;
  self.status := Books.fldStatus.Value;
  self.releseDate := Books.fldReleseDate.Value;
  self.pages := Books.fldPages.Value;
  self.price := Books.fldPrice.Value;
  self.currency := Books.fldCurrency.Value;
  self.imported := Books.fldImported.Value;
  self.description := Books.fldDescription.Value;
end;

procedure TBook.LoadFromJSON(jsBook: TJSONObject);
begin
  Status := jsBook.Values['status'].Value;
  Title := jsBook.Values['title'].Value;
  Isbn := jsBook.Values['isbn'].Value;
  Author := jsBook.Values['author'].Value;
  // ReleseDate := BooksToDateTime(jsBook.Values['date'].Value);
  Pages := (jsBook.Values['pages'] as TJSONNumber).AsInt;
  Price := StrToCurr(jsBook.Values['price'].Value);
  Currency := jsBook.Values['currency'].Value;
  Description := jsBook.Values['description'].Value;
  Imported := Now();
end;

procedure TBook.SetIsbn(const Value: string);
begin
  FIsbn := Value;
end;

procedure TBook.Validate;
var
  lValidateInfoMessage: TStringList;
begin
  lValidateInfoMessage := TStringList.Create;
  try
    //Regu³y walidacji dla ksi¹¿ki
    if not TValidateLibrary.IsNotEmpty(Isbn) then
      lValidateInfoMessage.Add('Waroœæ pola ISBN nie mo¿e byæ pusta!');
    if not TValidateLibrary.CheckIBAN(Isbn) then
      lValidateInfoMessage.Add('Waroœæ pola ISBN ma nieprawid³owy format!');

    if lValidateInfoMessage.Count <> 0 then
      raise Exception.Create(lValidateInfoMessage.Text);
  finally
    lValidateInfoMessage.Free;
  end;
end;

end.
