program EventBus;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {FormMain},
  Messaging.EventBus in 'Messaging.EventBus.pas',
  Global.MessagesID in 'Global.MessagesID.pas',
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
