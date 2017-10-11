unit mqttregister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mqttclient, mqttserver;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Bond',[TMQTTClient, TMQTTServer]);
end;

end.

