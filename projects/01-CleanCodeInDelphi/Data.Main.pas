unit Data.Main;

interface

uses
  System.SysUtils, System.Classes, Data.DB,
  // ------------------------------------------------------------------------
  // FireDAC: FDConnection:
  FireDAC.Comp.Client,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.Phys.Intf,
  FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.VCLUI.Wait,
  // ------------------------------------------------------------------------
  // FireDAC: SQLite:
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  // ------------------------------------------------------------------------
  // FireDAC: FDQuery:
  FireDAC.Comp.DataSet, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Stan.Async,
  // ------------------------------------------------------------------------
  Utils.Messages;

type
  TDataModMain = class(TDataModule)
    FDConnection1: TFDConnection;
    dsBooks: TFDQuery;
    dsReaders: TFDQuery;
    dsReports: TFDQuery;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
  private
  public
    procedure OpenDataSets;
    function FindReaderByEmil(const email: string): Variant;
  end;

var
  DataModMain: TDataModMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

uses
  System.Variants,
  ClientAPI.Books;

function TDataModMain.FindReaderByEmil(const email: string): Variant;
var
  ok: Boolean;
begin
  ok := dsReaders.Locate('email', email, []);
  if ok then
    Result := dsReaders.FieldByName('ReaderId').Value
  else
    Result := System.Variants.Null()
end;


procedure TDataModMain.OpenDataSets;
begin
  dsBooks.Open();
  dsReaders.Open();
  dsReports.Open();
end;

end.
