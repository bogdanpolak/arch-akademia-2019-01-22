unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.StdCtrls, Vcl.ExtCtrls,
  Data.Proxy.Book;

type
  TForm1 = class(TForm)
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    Button1: TButton;
    ListBox1: TListBox;
    GroupBox1: TGroupBox;
    Splitter1: TSplitter;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    BookProxy: TBookProxy;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Data.DataProxy;

procedure TForm1.FormCreate(Sender: TObject);
var
  ds: TDataSet;
begin
  BookProxy := TBookProxy.Create(Self);
  FDConnection1.ExecSQL('SELECT ISBN, Title, Authors, Status, ' +
    'ReleseDate, Pages, Price, Currency, Imported, Description FROM Books', ds);
  BookProxy.ConnectWithDataSet(ds);
  BookProxy.Open;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  BookProxy.ForEach(
    procedure(dsp: TDataSetProxy)
    begin
      ListBox1.Items.Add(BookProxy.ISBN.Value + ' ' + BookProxy.ToString);
    end);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Button2.Caption := BookProxy.CountNumberOfMoreExpensiveBooks.ToString;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  idx: Integer;
  line: string;
  ISBN: string;
begin
  idx := ListBox1.ItemIndex;
  if (idx >= 0) then
  begin
    line := ListBox1.Items[idx];
    ISBN := line.Substring(0,14);
    BookProxy.LocateISBN(ISBN);
    Caption := BookProxy.Title.Value;
  end;
end;

end.
