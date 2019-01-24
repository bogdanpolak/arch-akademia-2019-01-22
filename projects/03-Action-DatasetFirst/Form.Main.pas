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
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Mask,
  // -----
  Action.DataSet,
  Vcl.AppEvnts;

type
  TForm1 = class(TForm)
    FDConnection1: TFDConnection;
    fdqCustomers: TFDQuery;
    DBNavigator1: TDBNavigator;
    DBEdit1: TDBEdit;
    DBGrid1: TDBGrid;
    tmrReady: TTimer;
    Button1: TButton;
    GroupBox1: TGroupBox;
    btnDataSourceRemove: TButton;
    btnDataSourceCreate: TButton;
    btnBindAction: TButton;
    DataSource1: TDataSource;
    procedure btnBindActionClick(Sender: TObject);
    procedure btnDataSourceCreateClick(Sender: TObject);
    procedure btnDataSourceRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrReadyTimer(Sender: TObject);
  private
    actDataSetFirst: TDataSetFirstAction;
    procedure BindDBControlsToDataSet (DataSet: TDataSet);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  actDataSetFirst := TDataSetFirstAction.Create(Button1);
  actDataSetFirst.Caption := '⏪ First';
  actDataSetFirst.DataSource := DBNavigator1.DataSource;
  Button1.Action := actDataSetFirst;
end;

procedure TForm1.tmrReadyTimer(Sender: TObject);
begin
  tmrReady.Enabled := False;
  FDConnection1.Open();
  fdqCustomers.Open();
end;

procedure TForm1.BindDBControlsToDataSet (DataSet: TDataSet);
begin
  DBGrid1.DataSource := TDataSource.Create(Self);
  DBEdit1.DataSource := DBGrid1.DataSource;
  DBEdit1.DataField := 'CompanyName';
  DBNavigator1.DataSource := DBGrid1.DataSource;
  DBGrid1.DataSource.DataSet := DataSet;
end;

procedure TForm1.btnBindActionClick(Sender: TObject);
begin
  if DBGrid1.DataSource <> nil then
    actDataSetFirst.DataSource := DBGrid1.DataSource;
end;

procedure TForm1.btnDataSourceCreateClick(Sender: TObject);
begin
  if DBGrid1.DataSource = nil then
    BindDBControlsToDataSet(fdqCustomers);
end;

procedure TForm1.btnDataSourceRemoveClick(Sender: TObject);
var
  aDataSource: TDataSource;
begin
  if DBGrid1.DataSource <> nil then
  begin
    aDataSource := DBGrid1.DataSource;
    aDataSource.Free;
  end;
end;

end.
