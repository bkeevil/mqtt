{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mqttcomponents;

interface

uses
  mqttserver, mqttsubscriptions, mqtttokenizer, mqttmessages, mqttclient, 
  mqttpacketdefs, mqttpackets, mqttconsts, mqttregister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mqttregister', @mqttregister.Register);
end;

initialization
  RegisterPackage('mqttcomponents', @Register);
end.
