unit Messaging.EventBus;

interface

uses
  System.Classes;

type
  TEventMessage = record
  public
    TagInt: Integer;
    TagString: String;
    TagBoolean: boolean;
  end;

  TSubscriberEvent = procedure(MessageID: Integer;
    const AMessagee: TEventMessage) of object;

  TRegistrationInfo = record
    MessageID: Integer;
    method: TSubscriberEvent;
  end;

  TEventBus = class(TComponent)
  strict private
    Subscribers: TArray<TRegistrationInfo>;
    function LocateMethod(MessageID: Integer; AMethod: TSubscriberEvent)
      : Integer;
    class var GlobalEvenBus: TEventBus;
    class constructor Create;
    class destructor Destroy;
  public
    class procedure _Register(MessageID: Integer; AMethod: TSubscriberEvent);
    class procedure _Unregister(MessageID: Integer; AMethod: TSubscriberEvent);
    class procedure _Post(MessageID: Integer; const AMessage: TEventMessage);
    class procedure _Ping(MessageID: Integer);
    procedure RegisterMethod(MessageID: Integer; AMethod: TSubscriberEvent);
    procedure UnregisterMethod(MessageID: Integer; AMethod: TSubscriberEvent);
    procedure PostMessage(MessageID: Integer; const AMessage: TEventMessage);
    procedure PostPing(MessageID: Integer);
  end;

implementation

uses
  Vcl.Forms;

function SameMethod(AMethod1, AMethod2: TSubscriberEvent): boolean;
begin
  result := (TMethod(AMethod1).Code = TMethod(AMethod2).Code) and
    (TMethod(AMethod1).Data = TMethod(AMethod2).Data);
end;

class constructor TEventBus.Create;
begin
  GlobalEvenBus := TEventBus.Create(nil);
end;

class destructor TEventBus.Destroy;
begin
  GlobalEvenBus.Free;
  GlobalEvenBus := nil;
end;

function TEventBus.LocateMethod(MessageID: Integer;
  AMethod: TSubscriberEvent): Integer;
var
  count: Integer;
  i: Integer;
begin
  count := Length(Subscribers);
  for i := 0 to count - 1 do
    if (Subscribers[i].MessageID = MessageID) and
      SameMethod(Subscribers[i].method, AMethod) then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

procedure TEventBus.RegisterMethod(MessageID: Integer;
  AMethod: TSubscriberEvent);
var
  idx: Integer;
  count: Integer;
begin
  idx := LocateMethod(MessageID, AMethod);
  if idx < 0 then
  begin
    count := Length(Subscribers);
    SetLength(Subscribers, count + 1);
    Subscribers[count].MessageID := MessageID;
    Subscribers[count].method := AMethod;
  end;
end;

procedure TEventBus.UnregisterMethod(MessageID: Integer;
  AMethod: TSubscriberEvent);
var
  idx: Integer;
  count: Integer;
  i: Integer;
begin
  idx := LocateMethod(MessageID, AMethod);
  if idx >= 0 then
  begin
    count := Length(Subscribers);
    for i := idx to count - 2 do
      Subscribers[i] := Subscribers[i + 1];
    SetLength(Subscribers, count - 1);
  end;
end;

class procedure TEventBus._Register(MessageID: Integer;
  AMethod: TSubscriberEvent);
begin
  GlobalEvenBus.RegisterMethod(MessageID,AMethod);
end;

class procedure TEventBus._Unregister(MessageID: Integer;
  AMethod: TSubscriberEvent);
begin
  GlobalEvenBus.UnregisterMethod(MessageID,AMethod);
end;

class procedure TEventBus._Post(MessageID: Integer;
  const AMessage: TEventMessage);
begin
  GlobalEvenBus.PostMessage(MessageID,AMessage);
end;

class procedure TEventBus._Ping(MessageID: Integer);
begin
  GlobalEvenBus.PostPing(MessageID);
end;

procedure TEventBus.PostMessage(MessageID: Integer;
  const AMessage: TEventMessage);
var
  count: Integer;
  i: Integer;
begin
  count := Length(Subscribers);
  for i := 0 to count - 1 do
    if Subscribers[i].MessageID = MessageID then
      Subscribers[i].method(MessageID, AMessage);
end;

procedure TEventBus.PostPing(MessageID: Integer);
var
  mess: TEventMessage;
begin
  PostMessage(MessageID, mess);
end;

end.
