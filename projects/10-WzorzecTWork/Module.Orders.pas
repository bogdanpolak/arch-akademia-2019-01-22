unit Module.Orders;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.StorageBin, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, FireDAC.DApt;

type
  TModuleOrders = class(TDataModule)
    FDConnection1: TFDConnection;
    fdqOrders: TFDQuery;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
  private
  public
    function OpenNotShippedOrders(ReportOrdersRequiredBefore: TDateTime)
      : TDataSet;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}
{ TModuleOrders }

function TModuleOrders.OpenNotShippedOrders(ReportOrdersRequiredBefore
  : TDateTime): TDataSet;
var
  sql: string;
begin
  sql := 'SELECT OrderID, CustomerID, OrderDate, RequiredDate ' + sLineBreak +
    'FROM Orders ' + sLineBreak +
    'WHERE ShippedDate is NULL AND RequiredDate < :ADAY ' + sLineBreak +
    'ORDER BY RequiredDate';
 fdqOrders.Open( sql, [ReportOrdersRequiredBefore] );
 Result := fdqOrders;
end;

end.
