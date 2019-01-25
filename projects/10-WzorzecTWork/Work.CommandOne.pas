unit Work.CommandOne;

interface

uses
  System.Classes,
  Plus.TWork;

type
  TCommandOneState = (csInitialState, csPrepared, csWorking);

type
  TCommandOneWork = class(TWork)
  private
  public
    State: TCommandOneState;
    constructor Create(AOwner: TComponent); override;
    procedure Execute; override;
  end;

implementation

{ TCommandOneWork }

uses Plus.Vcl.Timer;

constructor TCommandOneWork.Create(AOwner: TComponent);
begin
  inherited;
  State := csInitialState;
end;

procedure TCommandOneWork.Execute;
begin
  inherited;
  if State = csInitialState then
  begin
    Self.Caption := 'Command is prepared and ready';
    State := csPrepared;
  end
  else if State = csPrepared then
  begin
    State := csWorking;
    Self.Caption := 'Working for 3 seconds (or a little more)';
    WorkAction.Enabled := False;
    Plus.Vcl.Timer.TEvenOnTimer.SetupOnce(Self, 3000,
      procedure
      begin
        State := csInitialState;
        Caption := 'Click to preapre';
        WorkAction.Enabled := True;
      end);
  end;
  WorkDone;
end;

end.
