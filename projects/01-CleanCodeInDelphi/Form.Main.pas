unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.JSON,
  Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Controls,
  ChromeTabs, ChromeTabsClasses, ChromeTabsTypes,
  Utils.General,
  ExtGUI.ListBox.Books;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    lbBooksReaded: TLabel;
    Splitter1: TSplitter;
    lbBooksAvaliable: TLabel;
    lbxBooksReaded: TListBox;
    lbxBooksAvaliable2: TListBox;
    ChromeTabs1: TChromeTabs;
    pnMain: TPanel;
    btnImport: TButton;
    tmrAppReady: TTimer;
    Splitter2: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure ChromeTabs1ButtonCloseTabClick(Sender: TObject; ATab: TChromeTab;
      var Close: Boolean);
    procedure ChromeTabs1Change(Sender: TObject; ATab: TChromeTab;
      TabChangeType: TTabChangeType);
    procedure FormResize(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure tmrAppReadyTimer(Sender: TObject);
  private
    FBooksConfig: TBooksListBoxConfigurator;
    procedure ResizeBooksListBoxesInsideGroupBox(aGroupBox: TGroupBox);
    // TODO 4: Move this procedure into class (idea)
    procedure ValidateBookAndGetDateReported(jsRow: TJSONObject; email: string;
      var dtReported: TDateTime);
    function AddChromeTabAndCreateFrame(AFrameClass: TFrameClass;
      ACaption: string): TFrame;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.StrUtils, System.Math, System.DateUtils,
  Data.DB,
  Vcl.DBGrids, Vcl.Graphics,
  // TODO 2: [!] FireDAC dependency (requred because of FDManager) (comments below)
  // Should be moved into the DataModule
  FireDAC.Comp.Client,
  // TODO 2: [!] FireDAC dependency (requred because of EFDDBEngineException)
  FireDAC.Stan.Error,
  // ------------------------------------------------------------
  Consts.Application,
  Utils.CipherAES128,
  Data.Main,
  ClientAPI.Readers,
  ClientAPI.Books,
  Frame.Welcome,
  Frame.Import,
  Helper.DataSet,
  Helper.TDBGrid,
  Helper.TApplication,
  Helper.TWinControl,
  Helper.TJSONObject;

const
  SecureKey = 'delphi-is-the-best';
  // SecurePassword = AES 128 ('masterkey',SecureKey)
  // SecurePassword = 'hC52IiCv4zYQY2PKLlSvBaOXc14X41Mc1rcVS6kyr3M=';
  // SecurePassword = AES 128 ('<null>',SecureKey)
  SecurePassword = 'EvUlRZOo3hzFEr/IRpHVMA==';
  Client_API_Token = '20be805d-9cea27e2-a588efc5-1fceb84d-9fb4b67c';
  SQL_GetDatabaseVersion = 'SELECT versionnr FROM DBInfo';

var
  ShowBooksGrid: Boolean = False;

resourcestring
  SWelcomeScreen = 'Welcome screen';
  SDBServerGone = 'Database server is gone';
  SDBConnectionUserPwdInvalid = 'Invalid database configuration.' +
    ' Application database user or password is incorrect.';
  SDBConnectionError = 'Can''t connect to database server. Unknown error.';
  SDBRequireCreate = 'Database is empty. You need to execute script' +
    ' creating required data.';
  SDBErrorSelect = 'Can''t execute SELECT command on the database';
  StrNotSupportedDBVersion = 'Not supported database version. Please' +
    ' update database structures.';

function DBVersionToString(VerDB: Integer): string;
begin
  Result := (VerDB div 1000).ToString + '.' + (VerDB mod 1000).ToString;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ResizeBooksListBoxesInsideGroupBox(GroupBox1);
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

{ TODO 2: [A] Method is too large. Comments is showing separate methods }
procedure TForm1.btnImportClick(Sender: TObject);
var
  frm: TFrameImport;
  jsData: TJSONArray;
  DBGrid1: TDBGrid;
  DataSrc1: TDataSource;
  DBGrid2: TDBGrid;
  DataSrc2: TDataSource;
  i: Integer;
  jsRow: TJSONObject;
  email: string;
  firstName: string;
  lastName: string;
  company: string;
  bookISBN: string;
  bookTitle: string;
  rating: Integer;
  oppinion: string;
  ss: array of string;
  v: string;
  dtReported: TDateTime;
  readerId: Variant;
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
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  frm := AddChromeTabAndCreateFrame(TFrameImport, 'Readers') as TFrameImport;
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // Dynamically Add TDBGrid to TFrameImport
  //
  { TODO 2: [C] Move code down separate bussines logic from GUI }
  // warning for dataset dependencies, discuss TDBGrid dependencies
  DataSrc1 := TDataSource.Create(frm);
  DBGrid1 := TDBGrid.Create(frm);
  DBGrid1.AlignWithMargins := True;
  DBGrid1.Parent := frm;
  DBGrid1.Align := alClient;
  DBGrid1.DataSource := DataSrc1;
  DataSrc1.DataSet := DataModMain.dsReaders;
  // AutoSizeColumns(DBGrid1);
  DBGrid1.AutoSizeColumns();
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // Import new Reader Reports data from OpenAPI
  // - Load JSON from WebService
  // - Validate JSON and insert new a Readers into the Database
  //
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
      { TODO 4: [A] Move this code into record TReaderReport.LoadFromJSON }
      jsRow := jsData.Items[i] as TJSONObject;
      email := jsRow.Values['email'].Value;
      if jsRow.FieldAvaliable('firstname') then
        firstName := jsRow.Values['firstname'].Value
      else
        firstName := '';
      if jsRow.FieldAvaliable('lastname') then
        lastName := jsRow.Values['lastname'].Value
      else
        lastName := '';
      if jsRow.FieldAvaliable('company') then
        company := jsRow.Values['company'].Value
      else
        company := '';
      if jsRow.FieldAvaliable('book-isbn') then
        bookISBN := jsRow.Values['book-isbn'].Value
      else
        bookISBN := '';
      if jsRow.FieldAvaliable('book-title') then
        bookTitle := jsRow.Values['book-title'].Value
      else
        bookTitle := '';
      if jsRow.FieldAvaliable('rating') then
        rating := (jsRow.Values['rating'] as TJSONNumber).AsInt
      else
        rating := -1;
      if jsRow.FieldAvaliable('oppinion') then
        oppinion := jsRow.Values['oppinion'].Value
      else
        oppinion := '';
      // ----------------------------------------------------------------
      //
      // Validate imported Reader report
      //
      { TODO 2: [E] Move validation up. Before reading data }
      ValidateBookAndGetDateReported(jsRow, email, dtReported);
      // ----------------------------------------------------------------
      //
      // Locate book by ISBN
      //
      { TODO 2: [G] Extract method }
      b := FBooksConfig.GetBookList(blkAll).FindByISBN(bookISBN);
      if not Assigned(b) then
        raise Exception.Create('Invalid book isbn');
      // ----------------------------------------------------------------
      // Find the Reader in then database using an email address
      readerId := DataModMain.FindReaderByEmil(email);
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
        DataModMain.dsReaders.AppendRecord([readerId, firstName, lastName,
          email, company, 1, dtReported, Now()]);
      end;
      // ----------------------------------------------------------------
      //
      // Append report into the database:
      // Fields: ReaderId, ISBN, Rating, Oppinion, Reported
      //
      DataModMain.dsReports.AppendRecord([readerId, bookISBN, rating, oppinion,
        dtReported]);
      // ----------------------------------------------------------------
      if Application.IsDeveloperMode then
        Insert([rating.ToString], ss, maxInt);
    end;
    // ----------------------------------------------------------------
    if Application.IsDeveloperMode then
      Caption := String.Join(' ,', ss);
    // ----------------------------------------------------------------
    with TSplitter.Create(frm) do
    begin
      Align := alBottom;
      Parent := frm;
      Height := 5;
    end;
    DBGrid1.Margins.Bottom := 0;
    DataSrc2 := TDataSource.Create(frm);
    DBGrid2 := TDBGrid.Create(frm);
    DBGrid2.AlignWithMargins := True;
    DBGrid2.Parent := frm;
    DBGrid2.Align := alBottom;
    DBGrid2.Height := frm.Height div 3;
    DBGrid2.DataSource := DataSrc2;
    DataSrc2.DataSet := DataModMain.dsReports;
    DBGrid2.Margins.Top := 0;
    // AutoSizeColumns(DBGrid2);
    DBGrid2.AutoSizeColumns();
  finally
    jsData.Free;
  end;
end;

procedure TForm1.ChromeTabs1ButtonCloseTabClick(Sender: TObject;
  ATab: TChromeTab; var Close: Boolean);
var
  obj: TObject;
begin
  obj := TObject(ATab.Data);
  (obj as TFrame).Free;
end;

procedure TForm1.ChromeTabs1Change(Sender: TObject; ATab: TChromeTab;
  TabChangeType: TTabChangeType);
var
  obj: TObject;
begin
  if Assigned(ATab) then
  begin
    obj := TObject(ATab.Data);
    if (TabChangeType = tcActivated) and Assigned(obj) then
    begin
      pnMain.HideAllChildFrames;
      (obj as TFrame).Visible := True;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pnMain.Caption := '';
end;

procedure TForm1.ResizeBooksListBoxesInsideGroupBox(aGroupBox: TGroupBox);
var
  sum: Integer;
  avaliable: Integer;
  labelPixelHeight: Integer;
begin
  { TODO 1: [too complicated] }
  // It's enought to do this:
  // lbxBooksReaded.Height :=
  // (lbxBooksReaded.Height + lbxBooksAvaliable2.Height) div 2;
  // method SumHeightForChildrens won't be used, but is intersting as a pattern
  with TBitmap.Create do
  begin
    Canvas.Font.Size := aGroupBox.Font.Height;
    labelPixelHeight := Canvas.TextHeight('Zg');
    Free;
  end;
  sum := aGroupBox.SumHeightForChildrens([lbxBooksReaded, lbxBooksAvaliable2]);
  avaliable := aGroupBox.Height - sum - labelPixelHeight;
  if aGroupBox.AlignWithMargins then
    avaliable := avaliable - aGroupBox.Padding.Top - aGroupBox.Padding.Bottom;
  if lbxBooksReaded.AlignWithMargins then
    avaliable := avaliable - lbxBooksReaded.Margins.Top -
      lbxBooksReaded.Margins.Bottom;
  if lbxBooksAvaliable2.AlignWithMargins then
    avaliable := avaliable - lbxBooksAvaliable2.Margins.Top -
      lbxBooksAvaliable2.Margins.Bottom;
  lbxBooksReaded.Height := avaliable div 2;
end;

procedure TForm1.ValidateBookAndGetDateReported(jsRow: TJSONObject;
  email: string; var dtReported: TDateTime);
begin
  if not CheckEmail(email) then
    raise Exception.Create('Invalid email addres');
  if not jsRow.IsValidIsoDateUtc('created', dtReported) then
    raise Exception.Create('Invalid date. Expected ISO format');
end;

// TODO 99: Usunąć z klasy TForm1 (Form.Main.pas)
function TForm1.AddChromeTabAndCreateFrame(AFrameClass: TFrameClass;
  ACaption: string): TFrame;
var
  tab: TChromeTab;
begin
  Result := AFrameClass.Create(pnMain);
  Result.Parent := pnMain;
  Result.Visible := True;
  Result.Align := alClient;
  tab := ChromeTabs1.Tabs.Add;
  tab.Caption := ACaption;
  tab.Data := Result;
end;

procedure TForm1.Splitter1Moved(Sender: TObject);
begin
  (Sender as TSplitter).Tag := 1;
end;

procedure TForm1.tmrAppReadyTimer(Sender: TObject);
var
  frm: TFrameWelcome;
  tab: TChromeTab;
  VersionNr: Integer;
  msg1: string;
  UserName: string;
  password: string;
  res: Variant;
  datasrc: TDataSource;
  DataGrid: TDBGrid;
begin
  tmrAppReady.Enabled := False;
  if Application.IsDeveloperMode then
    ReportMemoryLeaksOnShutdown := True;
  // ----------------------------------------------------------
  frm := AddChromeTabAndCreateFrame(TFrameWelcome, 'Welcome') as TFrameWelcome;
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // Connect to database server
  // Check application user and database structure (DB version)
  //
  // TODO 2: [!] Dependency on DataModMain.FDConnection1 (discuss LoD)
  // Move this code into the DataModule
  try
    UserName := FireDAC.Comp.Client.FDManager.ConnectionDefs.ConnectionDefByName
      (DataModMain.FDConnection1.ConnectionDefName).Params.UserName;
    password := AES128_Decrypt(SecurePassword, SecureKey);
    if password = '<null>' then
      password := '';
    DataModMain.FDConnection1.Open(UserName, password);
  except
    on E: FireDAC.Stan.Error.EFDDBEngineException do
    begin
      case E.kind of
        ekUserPwdInvalid:
          msg1 := SDBConnectionUserPwdInvalid;
        ekServerGone:
          msg1 := SDBServerGone;
      else
        msg1 := SDBConnectionError
      end;
      frm.AddInfo(0, msg1, True);
      frm.AddInfo(1, E.Message, False);
      exit;
    end;
  end;
  try
    res := DataModMain.FDConnection1.ExecSQLScalar(SQL_GetDatabaseVersion);
  except
    on E: EFDDBEngineException do
    begin
      msg1 := IfThen(E.kind = ekObjNotExists, SDBRequireCreate, SDBErrorSelect);
      frm.AddInfo(0, msg1, True);
      frm.AddInfo(1, E.Message, False);
      exit;
    end;
  end;
  VersionNr := res;
  if VersionNr <> ExpectedDatabaseVersionNr then
  begin
    frm.AddInfo(0, StrNotSupportedDBVersion, True);
    frm.AddInfo(1, 'Oczekiwana wersja bazy: ' +
      DBVersionToString(ExpectedDatabaseVersionNr), True);
    frm.AddInfo(1, 'Aktualna wersja bazy: ' + DBVersionToString
      (VersionNr), True);
  end;
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  DataModMain.OpenDataSets;
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // * Initialize ListBox'es for books
  // * Load books form database
  // * Setup drag&drop functionality for two list boxes
  // * Setup OwnerDraw mode
  //
  FBooksConfig := TBooksListBoxConfigurator.Create(self);
  FBooksConfig.PrepareListBoxes(lbxBooksReaded, lbxBooksAvaliable2);
  // ----------------------------------------------------------
  // ----------------------------`------------------------------
  //
  // Create Books Grid for Quality Tests
  if Application.IsDeveloperMode and ShowBooksGrid then
  begin
    datasrc := TDataSource.Create(frm);
    DataGrid := TDBGrid.Create(frm);
    DataGrid.AlignWithMargins := True;
    DataGrid.Parent := frm;
    DataGrid.Align := alClient;
    DataGrid.DataSource := datasrc;
    datasrc.DataSet := DataModMain.dsBooks;
    // AutoSizeColumns(DataGrid);
    DataGrid.AutoSizeColumns();
  end;
end;

end.
