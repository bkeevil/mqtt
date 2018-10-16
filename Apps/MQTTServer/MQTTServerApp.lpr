program MQTTServerApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, ServerFM, ServerPropertiesFM, mqttregister, lnetvisual,
  helpfm, createpasswordfm, PassManParamsFM;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TServerForm, ServerForm);
  Application.CreateForm(TServerPropertiesForm, ServerPropertiesForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
end.

