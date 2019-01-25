unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls,
  Messaging.EventBus;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure OnMessage(MessageID: Integer; const AMessagee: TEventMessage);
    procedure OnPause(MessageID: Integer; const AMessagee: TEventMessage);
  public
  end;

implementation

{$R *.dfm}

uses Global.MessagesID;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TEventBus._Unregister(EB_BOARD_StartScroll, OnMessage);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TEventBus._Register(EB_BOARD_StartScroll, OnMessage);
  TEventBus._Register(EB_BOARD_Pause, OnPause);
end;

procedure TForm1.OnMessage(MessageID: Integer; const AMessagee: TEventMessage);
begin
  ListBox1.Items.Add('['+TimeToStr( Now )+'] '+AMessagee.TagString);
end;

procedure TForm1.OnPause(MessageID: Integer; const AMessagee: TEventMessage);
begin
  ListBox1.Items.Add('['+TimeToStr( Now )+'] Pauza !!!');
end;


end.
