unit Action.DataSet;

interface

uses
  System.Classes,
  Data.DB,
  Vcl.ActnList;

type
  TDataSetFirstAction = class(TAction)
  private
    FDataSource: TDataSource;
    procedure SetDataSource(DataSource: TDataSource);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    FCountHandlesTarget: integer;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    property DataSource: TDataSource read FDataSource write SetDataSource;
  end;

implementation

// ------------------------------------------------------------------------
// TDataSetFirstAction
// ------------------------------------------------------------------------

procedure TDataSetFirstAction.ExecuteTarget(Target: TObject);
begin
  DataSource.DataSet.First;
end;

procedure TDataSetFirstAction.UpdateTarget(Target: TObject);
begin
  Enabled := (DataSource <> nil) and DataSource.DataSet.Active and
    not DataSource.DataSet.Bof;
end;

// TDataSource specyfic code

function TDataSetFirstAction.HandlesTarget(Target: TObject): Boolean;
var
  isAccepted: Boolean;
begin
  isAccepted := (DataSource <> nil) and (Target = DataSource) and
    (DataSource.DataSet <> nil);
  if isAccepted then
    FCountHandlesTarget := FCountHandlesTarget + 1;
  Result := isAccepted;
end;

procedure TDataSetFirstAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TDataSetFirstAction.SetDataSource(DataSource: TDataSource);
begin
  if DataSource <> FDataSource then
  begin
    FDataSource := DataSource;
    // TODO: Check this. Not sure why it's required
    if DataSource <> nil then
      DataSource.FreeNotification(Self);
  end;
end;

end.
