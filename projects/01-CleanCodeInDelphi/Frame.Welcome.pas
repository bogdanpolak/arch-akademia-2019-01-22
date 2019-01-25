unit Frame.Welcome;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Messaging.EventBus;

type
  TFrameWelcome = class(TFrame)
    Panel1: TPanel;
    lbAppName: TLabel;
    lbAppVersion: TLabel;
    tmrFrameReady: TTimer;
    Bevel1: TBevel;
    procedure tmrFrameReadyTimer(Sender: TObject);
  private
    procedure AddInfo(level: integer; const Msg: string;
      isInfoForDevelopers: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowInfo(MessageID: integer; const AMessagee: TEventMessage);
  end;

implementation

{$R *.dfm}

uses Consts.Application, Helper.TApplication;

procedure TFrameWelcome.AddInfo(level: integer; const Msg: string;
  isInfoForDevelopers: boolean);
var
  lbl: TLabel;
begin
  if not(isInfoForDevelopers) or Application.IsDeveloperMode then
  begin
    lbl := TLabel.Create(self);
    lbl.Top := Panel1.Height;
    Panel1.Height := Panel1.Height + lbl.Height;
    lbl.Align := alTop;
    lbl.AlignWithMargins := True;
    lbl.Parent := Panel1;
    if level > 0 then
      lbl.Caption := '* ' + Msg
    else
      lbl.Caption := Msg;
    if isInfoForDevelopers then
    begin
      lbl.Font.Style := [fsItalic];
      lbl.Font.Color := clGrayText;
    end;
    lbl.Margins.Left := 10 + Level * 20;
    lbl.Margins.Top := 0;
    lbl.Margins.Bottom := 0;
  end;
end;

constructor TFrameWelcome.Create(AOwner: TComponent);
begin
  inherited;
  TEventBus._Register(EB_DBCONNECTION_AddInfo, ShowInfo);
end;

destructor TFrameWelcome.Destroy;
begin
  inherited;
  TEventBus._Unregister(EB_DBCONNECTION_AddInfo, ShowInfo);
end;

procedure TFrameWelcome.ShowInfo(MessageID: integer;
  const AMessagee: TEventMessage);
begin
  Self.AddInfo(AMessagee.TagInt, AMessagee.TagString, AMessagee.TagBoolean);
end;

procedure TFrameWelcome.tmrFrameReadyTimer(Sender: TObject);
begin
  tmrFrameReady.Enabled := false;
  lbAppName.Caption := Consts.Application.ApplicationName;
  lbAppVersion.Caption := Consts.Application.ApplicationVersion;
end;

end.
