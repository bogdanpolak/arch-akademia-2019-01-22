unit Helper.DataSet;

interface

uses
  Data.DB;

type
  TDataSetHelper = class helper for TDataSet
    function GetMaxValue(const fieldName: string): integer;
  end;

implementation

{ TDataSetHelper }

function TDataSetHelper.GetMaxValue(const fieldName: string): integer;
var
  v: integer;
begin
  Result := 0;
  self.DisableControls;
  self.First;
  while not self.Eof do
  begin
    v := self.FieldByName(fieldName).AsInteger;
    if v > Result then
      Result := v;
    self.Next;
  end;
  self.EnableControls;
end;

end.
