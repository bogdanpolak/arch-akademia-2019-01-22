program WzorzecTWork;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Work.CommandOne in 'Work.CommandOne.pas',
  Work.Async.NotShippedOrders in 'Work.Async.NotShippedOrders.pas',
  Module.Orders in 'Module.Orders.pas' {ModuleOrders: TDataModule},
  Plus.TWork in 'Plus.TWork.pas',
  Plus.Vcl.Timer in 'Plus.Vcl.Timer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
