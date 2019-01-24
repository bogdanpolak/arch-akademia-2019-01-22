unit ExtGUI.ListBox.Books;

interface

uses
  System.Classes, Vcl.StdCtrls, Vcl.Controls, System.Types, Vcl.Graphics,
  Winapi.Windows,
  System.JSON,
  DataAccess.Books,
  Model.Book,
  Model.BookCollection;

type
  { TODO 4: Too many responsibilities. Separate GUI from structures }
  // Split into 2 classes TBooksContainer TListBoxesForBooks
  // Add new unit: Model.Books.pas
  TBooksListBoxConfigurator = class(TComponent)
  private
    FAllBooks: TBookCollection;
    FListBoxOnShelf: TListBox;
    FListBoxAvaliable: TListBox;
    FBooksOnShelf: TBookCollection;
    FBooksAvaliable: TBookCollection;
    DragedIdx: integer;
    procedure EventOnStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure EventOnDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure EventOnDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: Boolean);
    procedure EventOnDrawItem(Control: TWinControl; Index: integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure AddBookToAvaliableOrOnShelfLits(b: TBook);
    procedure ConfigureBookListBox(ABookList: TBookCollection;
      AListBox: TListBox);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { TODO 3: Introduce 3 properties: ListBoxOnShelf, ListBoxAvaliable, Books }
    procedure PrepareListBoxes(lbxOnShelf, lbxAvaliable: TListBox);
    function GetBookList(kind: TBookListKind): TBookCollection;
    function FindBook(isbn: string): TBook;
    procedure InsertNewBook(b: TBook);
  end;

implementation

uses
  System.SysUtils,
  DataAccess.Books.FireDAC,
  Data.Main;

constructor TBooksListBoxConfigurator.Create(AOwner: TComponent);
var
  b: TBook;
  BooksDAO: IBooksDAO;
begin
  inherited;
  // ---------------------------------------------------
  FAllBooks := TBookCollection.Create();
  { TODO 5: Discuss how to remove this dependency. Check implentation uses }
  BooksDAO := GetBooks_FireDAC(DataModMain.dsBooks);
  FAllBooks.LoadDataSet(BooksDAO);
  // ---------------------------------------------------
  FBooksOnShelf := TBookCollection.Create(false);
  FBooksAvaliable := TBookCollection.Create(false);
  for b in FAllBooks do
    AddBookToAvaliableOrOnShelfLits(b);
end;

destructor TBooksListBoxConfigurator.Destroy;
begin
  FAllBooks.Free;
  FBooksOnShelf.Free;
  FBooksAvaliable.Free;
  inherited;
end;

function TBooksListBoxConfigurator.GetBookList(kind: TBookListKind)
  : TBookCollection;
begin
  case kind of
    blkAll:
      Result := FAllBooks;
    blkOnShelf:
      Result := FBooksOnShelf;
    blkAvaliable:
      Result := FBooksAvaliable
  else
    raise Exception.Create('Not supported collection type');
  end;
end;

procedure TBooksListBoxConfigurator.InsertNewBook(b: TBook);
begin
  FAllBooks.Add(b);
  AddBookToAvaliableOrOnShelfLits(b);
  FListBoxAvaliable.AddItem(b.title, b);
end;

procedure TBooksListBoxConfigurator.ConfigureBookListBox
  (ABookList: TBookCollection; AListBox: TListBox);
var
  b: TBook;
begin
  for b in ABookList do
    AListBox.AddItem(b.title, b);
  AListBox.OnDragDrop := EventOnDragDrop;
  AListBox.OnDragOver := EventOnDragOver;
  AListBox.OnStartDrag := EventOnStartDrag;
  AListBox.OnDrawItem := EventOnDrawItem;
  AListBox.Style := lbOwnerDrawFixed;
  AListBox.DragMode := dmAutomatic;
  AListBox.ItemHeight := 50;
end;

procedure TBooksListBoxConfigurator.AddBookToAvaliableOrOnShelfLits(b: TBook);
begin
  if b.status = 'on-shelf' then
    FBooksOnShelf.Add(b)
  else if b.status = 'avaliable' then
    FBooksAvaliable.Add(b);
end;

function TBooksListBoxConfigurator.FindBook(isbn: string): TBook;
begin
  Result := FAllBooks.FindByISBN(isbn);
end;

procedure TBooksListBoxConfigurator.PrepareListBoxes(lbxOnShelf,
  lbxAvaliable: TListBox);
begin
  FListBoxOnShelf := lbxOnShelf;
  FListBoxAvaliable := lbxAvaliable;
  ConfigureBookListBox(FBooksOnShelf, lbxOnShelf);
  ConfigureBookListBox(FBooksAvaliable, lbxAvaliable);
end;

procedure TBooksListBoxConfigurator.EventOnStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  lbx: TListBox;
begin
  lbx := Sender as TListBox;
  DragedIdx := lbx.ItemIndex;
end;

procedure TBooksListBoxConfigurator.EventOnDragDrop(Sender, Source: TObject;
  X, Y: integer);
var
  lbx2: TListBox;
  lbx1: TListBox;
  b: TBook;
  srcList: TBookCollection;
  dstList: TBookCollection;
begin
  lbx1 := Source as TListBox;
  lbx2 := Sender as TListBox;
  b := lbx1.Items.Objects[DragedIdx] as TBook;
  if lbx1 = FListBoxOnShelf then
  begin
    srcList := FBooksOnShelf;
    dstList := FBooksAvaliable;
  end
  else
  begin
    srcList := FBooksAvaliable;
    dstList := FBooksOnShelf;
  end;
  dstList.Add(srcList.Extract(b));
  lbx1.Items.Delete(DragedIdx);
  lbx2.AddItem(b.title, b);
end;

procedure TBooksListBoxConfigurator.EventOnDragOver(Sender, Source: TObject;
  X, Y: integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TListBox) and (DragedIdx >= 0) and (Sender <> Source);
end;

procedure TBooksListBoxConfigurator.EventOnDrawItem(Control: TWinControl;
  Index: integer; Rect: TRect; State: TOwnerDrawState);
var
  s: string;
  ACanvas: TCanvas;
  b: TBook;
  r2: TRect;
  lbx: TListBox;
  colorTextTitle: integer;
  colorTextAuthor: integer;
  colorBackground: integer;
  colorGutter: integer;
begin
  // TOwnerDrawState = set of (odSelected, odGrayed, odDisabled, odChecked,
  // odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect,
  // odReserved1, odReserved2, odComboBoxEdit);
  lbx := Control as TListBox;

  // if (odSelected in State) and (odFocused in State) then
  if (odSelected in State) then
  begin
    colorGutter := $F0FFD0;
    colorTextTitle := clHighlightText;
    colorTextAuthor := $FFFFC0;
    colorBackground := clHighlight;
  end
  else
  begin
    colorGutter := $A0FF20;
    colorTextTitle := lbx.Font.Color;
    colorTextAuthor := $909000;
    colorBackground := lbx.Color;
  end;
  b := lbx.Items.Objects[Index] as TBook;
  s := b.title;
  ACanvas := lbx.Canvas;
  ACanvas.Brush.Color := colorBackground;
  r2 := Rect;
  r2.Left := 0;
  ACanvas.FillRect(r2);
  ACanvas.Brush.Color := colorGutter;
  r2 := Rect;
  r2.Left := 0;
  InflateRect(r2, -3, -5);
  r2.Right := r2.Left + 6;
  ACanvas.FillRect(r2);
  ACanvas.Brush.Color := colorBackground;
  Rect.Left := Rect.Left + 13;
  ACanvas.Font.Color := colorTextAuthor;
  ACanvas.Font.Size := lbx.Font.Size;
  ACanvas.TextOut(13, Rect.Top + 2, b.author);
  r2 := Rect;
  r2.Left := 13;
  r2.Top := r2.Top + ACanvas.TextHeight('Ag');
  ACanvas.Font.Color := colorTextTitle;
  ACanvas.Font.Size := lbx.Font.Size + 2;
  InflateRect(r2, -2, -1);
  DrawText(ACanvas.Handle, PChar(s), Length(s), r2,
    // DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
    DT_LEFT or DT_WORDBREAK);
end;

end.
