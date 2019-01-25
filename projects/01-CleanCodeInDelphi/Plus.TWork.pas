unit Plus.TWork;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  VCL.ActnList;

type
  TWork = class;

  EInvalidWork = class(Exception);

  TWorkActionEvent = procedure(Sender: TObject; AWork: TWork) of object;

  TClassOfWork = class of TWork;

  TWorkAction = class(TAction)
  private
    FWorkList: TObjectList<TWork>;
    FOnWorkStarted: TWorkActionEvent;
    FOnWorkDone: TWorkActionEvent;
    procedure SetOnWorkStarted(const Event: TWorkActionEvent);
    procedure SetOnWorkDone(const Event: TWorkActionEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddWork(AWork: TWork): TWork;
    function CreateAndAddWork (WorkClas: TClassOfWork): TWork;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure WorkDone(Work: TWork);
    procedure WorkStarted(Work: TWork);
    property OnWorkStarted: TWorkActionEvent read FOnWorkStarted
      write SetOnWorkStarted;
    property OnWorkDone: TWorkActionEvent read FOnWorkDone write SetOnWorkDone;
  end;

  TWork = class(TComponent)
  private
    FWorkAction: TWorkAction;
    procedure SetWorkAction(const aWorkAction: TWorkAction);
    procedure ValidateIfWorkActionIsSet;
    function GetCaption: string;
    procedure SetCaption(ACaption: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WorkDone;
    procedure Execute; virtual;
    property WorkAction: TWorkAction read FWorkAction write SetWorkAction;
    property Caption: String read GetCaption write SetCaption;
  end;

implementation

// -------------------------------------------------------------------
// -------------------------------------------------------------------
// TWork

constructor TWork.Create(AOwner: TComponent);
begin
  if not(AOwner is TWorkAction) then
    raise EInvalidWork.Create
      ('TWork cant be created. TWorkAction is required as TWork Owner');
  inherited;
  WorkAction := (AOwner as TWorkAction);
end;

destructor TWork.Destroy;
begin

  inherited;
end;

procedure TWork.WorkDone;
begin
  WorkAction.WorkDone(Self);
end;

procedure TWork.Execute;
begin
  WorkAction.WorkStarted(Self);
end;

procedure TWork.SetWorkAction(const aWorkAction: TWorkAction);
begin
  FWorkAction := aWorkAction;
end;

procedure TWork.ValidateIfWorkActionIsSet;
begin
  if FWorkAction = nil then
    raise EInvalidWork.Create
      ('TWorkAction is required. Set property TWork.WorkAction.');
end;

function TWork.GetCaption: string;
begin
  ValidateIfWorkActionIsSet;
  Result := WorkAction.Caption;
end;

procedure TWork.SetCaption(ACaption: string);
begin
  ValidateIfWorkActionIsSet;
  WorkAction.Caption := ACaption;
end;


// -------------------------------------------------------------------
// -------------------------------------------------------------------
// TWorkAction

constructor TWorkAction.Create(AOwner: TComponent);
begin
  inherited;
  FWorkList := TObjectList<TWork>.Create;
end;

destructor TWorkAction.Destroy;
begin
  FWorkList.Free;
  inherited;
end;

function TWorkAction.AddWork(AWork: TWork): TWork;
begin
  FWorkList.Add(AWork);
  Result := AWork;
end;

function TWorkAction.CreateAndAddWork (WorkClas: TClassOfWork): TWork;
var
  AWork: TWork;
begin
  AWork := WorkClas.Create(Self);
  Self.AddWork(AWork);
  Result := AWork;
end;

function TWorkAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (FWorkList.Count>0);
end;

procedure TWorkAction.ExecuteTarget(Target: TObject);
var
  Work: TWork;
begin
  inherited;
  for Work in FWorkList do
    Work.Execute;
end;

procedure TWorkAction.SetOnWorkStarted(const Event: TWorkActionEvent);
begin
  FOnWorkStarted := Event;
end;

procedure TWorkAction.SetOnWorkDone(const Event: TWorkActionEvent);
begin
  FOnWorkDone := Event;
end;

procedure TWorkAction.WorkStarted(Work: TWork);
begin
  if Assigned(FOnWorkStarted) then
    FOnWorkStarted(Self, Work);
end;

procedure TWorkAction.WorkDone(Work: TWork);
begin
  if Assigned(FOnWorkDone) then
    FOnWorkDone(Self, Work);
end;

end.
