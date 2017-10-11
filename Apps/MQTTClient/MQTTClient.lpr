program MQTTClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, connectfm,
  clientfm, publishfm, mqttregister, subscribefm, lnetvisual;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TClientForm, ClientForm);
  Application.CreateForm(TConnectDlg, ConnectDlg);
  Application.CreateForm(TPublishForm, PublishForm);
  Application.CreateForm(TSubscribeForm, SubscribeForm);
  Application.Run;
end.

