unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Messaging.EventBus, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    tmrAnimate: TTimer;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure tmrAnimateTimer(Sender: TObject);
  private
    DefualtTimerInterval: Integer;
    procedure EvenBusOnStartScroll(MessageID: Integer;
      const AMessage: TEventMessage);
    procedure EvenBusOnChangeColor(MessageID: Integer;
      const AMessage: TEventMessage);
    procedure EvenBusOnChangeSpeed(MessageID: Integer;
      const AMessage: TEventMessage);
    procedure EvenBusOnAnimate(MessageID: Integer;
      const AMessage: TEventMessage);
    procedure EvenBusOnPause(MessageID: Integer;
      const AMessage: TEventMessage);
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Global.MessagesID;

procedure TForm2.EvenBusOnStartScroll(MessageID: Integer;
  const AMessage: TEventMessage);
begin
  tmrAnimate.Enabled := False;
  Label1.Top := 8;
  Label1.Font.Size := 60;
  Label1.Caption := AMessage.TagString;
  Label1.Visible := true;
  Label1.Left := self.ClientWidth - 20;
  tmrAnimate.Enabled := True;
end;

procedure TForm2.EvenBusOnChangeSpeed(MessageID: Integer;
  const AMessage: TEventMessage);
begin
  if AMessage.TagBoolean then
    tmrAnimate.Interval := DefualtTimerInterval div 2
  else
    tmrAnimate.Interval := DefualtTimerInterval;
end;

procedure TForm2.EvenBusOnChangeColor(MessageID: Integer;
  const AMessage: TEventMessage);
begin
  // Label1->Font->Color = (TColor) message->TagInt;
  Label1.Font.Color := AMessage.TagInt;
end;

procedure TForm2.EvenBusOnAnimate(MessageID: Integer;
  const AMessage: TEventMessage);
begin
  tmrAnimate.Enabled := True;
end;

procedure TForm2.EvenBusOnPause(MessageID: Integer;
  const AMessage: TEventMessage);
begin
  tmrAnimate.Enabled := False;
end;

// --------------------------------------------------------------------------

procedure TForm2.FormCreate(Sender: TObject);
begin
  self.DoubleBuffered := True;
  DefualtTimerInterval := 60;
  tmrAnimate.Interval := DefualtTimerInterval;
  tmrAnimate.Enabled := False;
  Label1.Visible := false;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  TEventBus._Register(EB_BOARD_StartScroll, self.EvenBusOnStartScroll);
  TEventBus._Register(EB_BOARD_ChangeSpeed, self.EvenBusOnChangeSpeed);
  TEventBus._Register(EB_BOARD_ChangeColor, self.EvenBusOnChangeColor);
  TEventBus._Register(EB_BOARD_Animate, self.EvenBusOnAnimate);
  TEventBus._Register(EB_BOARD_Pause, self.EvenBusOnPause);
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TEventBus._Unregister(EB_BOARD_StartScroll, self.EvenBusOnStartScroll);
  TEventBus._Unregister(EB_BOARD_ChangeSpeed, self.EvenBusOnChangeSpeed);
  TEventBus._Unregister(EB_BOARD_ChangeColor, self.EvenBusOnChangeColor);
  TEventBus._Unregister(EB_BOARD_Animate, self.EvenBusOnAnimate);
  TEventBus._Unregister(EB_BOARD_Pause, self.EvenBusOnPause);
end;

procedure TForm2.tmrAnimateTimer(Sender: TObject);
begin
  Label1.Left := Label1.Left - 5;
end;

end.
