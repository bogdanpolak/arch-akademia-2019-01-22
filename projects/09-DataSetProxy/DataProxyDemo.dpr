program DataProxyDemo;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Data.DataProxy in 'Data.DataProxy.pas',
  Data.Proxy.Book in 'Data.Proxy.Book.pas',
  Helper.TDataSet in 'Helper.TDataSet.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
