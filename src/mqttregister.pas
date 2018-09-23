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
  RegisterComponents('MQTT',[TMQTTClient, TMQTTServer]);
end;

end.

