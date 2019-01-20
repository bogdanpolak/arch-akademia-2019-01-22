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
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  // ------------------------------------------------------------------------
  // FireDAC: FDQuery:
  FireDAC.Comp.DataSet, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Stan.Async,
  // ------------------------------------------------------------------------
  // ------------------------------------------------------------------------
  // Project units:
  Utils.Messages, FireDAC.Phys;

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
    function FindReaderByEmil (const email: string): Variant;
    { TODO 2: [Helper] Extract into TDataSet helper. This pollutes the Data Module public API }
    function GetMaxValueInDataSet(DataSet: TDataSet;
      const fieldName: string): integer;
  end;

var
  DataModMain: TDataModMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

uses
  System.Variants, ClientAPI.Books;


{ TODO 1: Commented out function. Just delete it }
{
function BooksToDateTime(const s: string): TDateTime;
const
  months: array [1 .. 12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  m: string;
  y: string;
  i: integer;
  mm: integer;
  yy: integer;
begin
  m := s.Substring(0, 3);
  y := s.Substring(4);
  mm := 0;
  for i := 1 to 12 do
    if months[i].ToUpper = m.ToUpper then
      mm := i;
  if mm = 0 then
    raise ERangeError.Create('Incorect month name in the date: ' + s);
  yy := y.ToInteger();
  Result := EncodeDate(yy, mm, 1);
end;
}

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

function TDataModMain.GetMaxValueInDataSet(DataSet: TDataSet;
  const fieldName: string): integer;
var
  v: Integer;
begin
  { TODO 2: [Helper] Extract into TDBGrid.ForEachRow class helper }
  Result := 0;
  DataSet.DisableControls;
  DataSet.First;
  while not DataSet.Eof do
  begin
    v := DataSet.FieldByName(fieldName).AsInteger;
    if v>Result then
      Result := v;
    DataSet.Next;
  end;
  DataSet.EnableControls;
end;

procedure TDataModMain.OpenDataSets;
var
  JSONFileName: string;
  fname: string;
  days: integer;
  half: Int64;
  ms: TMemoryStream;
  tab: TFDMemTable;
  j: integer;
  recNo: integer;
  email: string;
begin
  dsBooks.Open();
  dsReaders.Open();
  dsReports.Open();
end;

end.
