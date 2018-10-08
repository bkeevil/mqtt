unit mqttregister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mqttclient, mqttserver;

procedure Register;

implementation

uses
  LResources;

procedure Register;
begin
  {$I mqttserver_icon.lrs}
  {$I mqttclient_icon.lrs}
  {$I mqttclientsubscription_icon.lrs}
  {$I mqttclientpublisher_icon.lrs}
  RegisterComponents('MQTT',[TMQTTClient, TMQTTClientSubscription, TMQTTClientPublisher, TMQTTServer]);
end;

end.

