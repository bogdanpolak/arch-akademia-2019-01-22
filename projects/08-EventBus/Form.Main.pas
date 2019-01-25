unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    GroupBox1: TGroupBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    btnPostMessage1: TButton;
    btnShowSubscribers: TButton;
    Edit1: TEdit;
    chkFastAnimataion: TCheckBox;
    btnExit: TButton;
    ColorBox1: TColorBox;
    GridPanel1: TGridPanel;
    btnPause: TButton;
    btnAnimate: TButton;
    tmrReady: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnAnimateClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnPostMessage1Click(Sender: TObject);
    procedure btnShowSubscribersClick(Sender: TObject);
    procedure chkFastAnimataionClick(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure Edit1DblClick(Sender: TObject);
    procedure tmrReadyTimer(Sender: TObject);
  private
    procedure UpdateControlsActivity(IsEnable: boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses Messaging.EventBus, Global.MessagesID, Unit1, Unit2;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TFormMain.UpdateControlsActivity(IsEnable: boolean);
begin
  btnShowSubscribers.Enabled := not IsEnable;
  btnPostMessage1.Enabled := IsEnable;
  Edit1.Enabled := IsEnable;
  chkFastAnimataion.Enabled := IsEnable;
  ColorBox1.Enabled := IsEnable;
  btnPause.Enabled := IsEnable;
  btnAnimate.Enabled := IsEnable;
end;

procedure TFormMain.btnShowSubscribersClick(Sender: TObject);
var
  frm1: TForm1;
  frm2: TForm2;
begin
  UpdateControlsActivity(True);
  // ---
  frm1 := TForm1.Create(Application);
  frm1.Visible := True;
  frm1.Left := Left + Width;
  frm1.Top := Top;
  frm1.Show();
  // ---
  frm2 := TForm2.Create(Application);
  frm2.Visible := True;
  frm2.Left := frm1.Left + frm1.Width;
  frm2.Top := Top;
  frm2.Show();
end;

procedure TFormMain.btnPostMessage1Click(Sender: TObject);
var
  AMessage: TEventMessage;
begin
  AMessage.TagString := Edit1.Text;
  TEventBus._Post(EB_BOARD_StartScroll, AMessage);
end;

procedure TFormMain.chkFastAnimataionClick(Sender: TObject);
var
  AMessage: TEventMessage;
begin
  AMessage.TagBoolean := chkFastAnimataion.Checked;
  TEventBus._Post(EB_BOARD_ChangeSpeed, AMessage);
end;

procedure TFormMain.ColorBox1Change(Sender: TObject);
var
  AMessage: TEventMessage;
begin
  AMessage.TagInt := ColorBox1.Selected;
  TEventBus._Post(EB_BOARD_ChangeColor, AMessage);
end;

procedure TFormMain.btnPauseClick(Sender: TObject);
begin
  TEventBus._Ping(EB_BOARD_Pause);
end;

procedure TFormMain.btnAnimateClick(Sender: TObject);
begin
  TEventBus._Ping(EB_BOARD_Animate);
end;

procedure TFormMain.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.Edit1DblClick(Sender: TObject);
begin
  Edit1.SelectAll;
end;

procedure TFormMain.tmrReadyTimer(Sender: TObject);
begin
  tmrReady.Enabled := False;
  Self.UpdateControlsActivity(False);
end;

end.
