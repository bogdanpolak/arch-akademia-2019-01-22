program CleanCodeDemo;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Frame.Welcome in 'Frame.Welcome.pas' {FrameWelcome: TFrame},
  Fake.FDConnection in 'Fake.FDConnection.pas',
  Consts.Application in 'Consts.Application.pas',
  Utils.CipherAES128 in 'Utils.CipherAES128.pas',
  Frame.Import in 'Frame.Import.pas' {FrameImport: TFrame},
  Utils.General in 'Utils.General.pas',
  Data.Main in 'Data.Main.pas' {DataModMain: TDataModule},
  Vcl.Themes,
  Vcl.Styles,
  DataAccess.Base in 'experimental\DataAccess.Base.pas',
  DataAccess.Books in 'experimental\DataAccess.Books.pas',
  DataAccess.Books.FireDAC in 'experimental\DataAccess.Books.FireDAC.pas',
  ClientAPI.Books in 'api\ClientAPI.Books.pas',
  ClientAPI.Readers in 'api\ClientAPI.Readers.pas',
  ExtGUI.ListBox.Books in 'ExtGUI.ListBox.Books.pas',
  Helper.DataSet in 'Helper.DataSet.pas',
  Helper.TDBGrid in 'Helper.TDBGrid.pas',
  Helper.TApplication in 'Helper.TApplication.pas',
  Helper.TWinControl in 'Helper.TWinControl.pas',
  Helper.TJSONObject in 'Helper.TJSONObject.pas',
  Helper.Variant in 'Helper.Variant.pas',
  Action.ImportFromWebService in 'Action.ImportFromWebService.pas',
  Model.Book in 'Model.Book.pas',
  Model.BookCollection in 'Model.BookCollection.pas',
  Model.ReaderReport in 'Model.ReaderReport.pas',
  Messaging.EventBus in 'Messaging.EventBus.pas',
  Plus.TWork in 'Plus.TWork.pas',
  Work.ImportReadReports in 'Work.ImportReadReports.pas',
  Work.ImportBooks in 'Work.ImportBooks.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModMain, DataModMain);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
