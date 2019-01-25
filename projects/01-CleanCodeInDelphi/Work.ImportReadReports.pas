unit Work.ImportReadReports;

interface

uses
  Plus.TWork,
  ExtGUI.ListBox.Books,
  Data.Main;

type
  TImportReadReportsWork = class (TWork)
  private
    FBooksConfig: TBooksListBoxConfigurator;
    procedure DoImportReaderReports;
    procedure SetBooksConfig(const Value: TBooksListBoxConfigurator);
  public
    procedure Execute; override;
    property BooksConfig: TBooksListBoxConfigurator read FBooksConfig write SetBooksConfig;
  end;

implementation


{ TImportReadReportsWork }

uses
  System.JSON,
  System.Variants,
  System.SysUtils,
  Vcl.Forms,
  ClientAPI.Readers,
  Messaging.EventBus,
  Model.Book,
  Model.ReaderReport,
  Consts.Application,
  Helper.DataSet,
  Helper.TApplication;

procedure TImportReadReportsWork.DoImportReaderReports;
var
  jsData: TJSONArray;
  I: Integer;
  ReaderReport: TReaderReport;
  Book: TBook;
  Ratings: array of string;
  ReaderId: Variant;
  AMessage: TEventMessage;
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
      if VarIsNull(ReaderId) then
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
      AMessage.TagString := string.Join(' ,', Ratings);
      TEventBus._Post (EB_ImportReaderReports_LogInfo, AMessage);
    end;
  finally
    jsData.Free;
  end;
  WorkDone;
end;

procedure TImportReadReportsWork.Execute;
begin
  inherited;
  DoImportReaderReports;
end;

procedure TImportReadReportsWork.SetBooksConfig(
  const Value: TBooksListBoxConfigurator);
begin
  FBooksConfig := Value;
end;

end.
