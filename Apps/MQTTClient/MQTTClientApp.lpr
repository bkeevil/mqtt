program MQTTClientApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lnetvisual, clientfm, connectfm, publishfm, subscribefm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TClientForm, ClientForm);
  Application.CreateForm(TMQTTConnectDlg, MQTTConnectDlg);
  Application.CreateForm(TPublishForm, PublishForm);
  Application.CreateForm(TSubscribeForm, SubscribeForm);
  Application.Run;
end.

