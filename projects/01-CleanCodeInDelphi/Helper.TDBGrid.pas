unit Helper.TDBGrid;

interface

uses
  Data.DB,
  System.Classes,
  System.Math,
  Vcl.DBGrids;

type
  TDBGridHelper = class helper for TDBGrid
  public
    /// <summary>
    /// Funkcja przegl¹da ustaln¹ liczbê wierszy DataSet-a pod³¹czonego
    /// do DBGrid-a i dla ka¿dego wiesza wylicza maksymaln¹ szrokoœæ
    /// kolumny w pikselach.
    /// </summary>
    /// <param name="MaxRow">
    /// Maksymalna liczba wierszy w DataSet-cie, dla których wyliczana
    /// jest szerokoœæ kolumn. Domyœlnie = 25 wierszy
    /// </param>
    procedure AutoSizeColumns(const MaxRows: Integer = 25);
  end;

implementation

procedure TDBGridHelper.AutoSizeColumns(const MaxRows: Integer = 25);
var
  DataSet: Data.DB.TDataSet;
  Bookmark: TBookmark;
  Count, i: Integer;
  ColumnsWidth: array of Integer;
begin
  SetLength(ColumnsWidth, Self.Columns.Count);
  for i := 0 to Self.Columns.Count - 1 do
    if Self.Columns[i].Visible then
      ColumnsWidth[i] := Self.Canvas.TextWidth
        (Self.Columns[i].title.Caption + '   ')
    else
      ColumnsWidth[i] := 0;
  if Self.DataSource <> nil then
    DataSet := Self.DataSource.DataSet
  else
    DataSet := nil;
  if (DataSet <> nil) and DataSet.Active then
  begin
    Bookmark := DataSet.GetBookmark;
    try
      DataSet.DisableControls;
      try
        Count := 0;
        DataSet.First;
        while not DataSet.Eof and (Count < MaxRows) do
        begin
          for i := 0 to Self.Columns.Count - 1 do
            if Self.Columns[i].Visible then
              ColumnsWidth[i] := Max(ColumnsWidth[i],
                Self.Canvas.TextWidth(Self.Columns[i].Field.Text + '   '));
          Inc(Count);
          DataSet.Next;
        end;
      finally
        DataSet.EnableControls;
      end;
    finally
      if DataSet.BookmarkValid(Bookmark) then
        DataSet.GotoBookmark(Bookmark);
      DataSet.FreeBookmark(Bookmark);
    end;
  end;
  for i := 0 to Self.Columns.Count - 1 do
    if Self.Columns[i].Visible then
      Self.Columns[i].Width := ColumnsWidth[i];
end;

end.
