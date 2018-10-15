program MQTTServerApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ServerFM, ServerPropertiesFM, mqttregister, lnetvisual, helpfm,
  createpasswordfm, passmanfm, PassManParamsFM;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TServerForm, ServerForm);
  Application.CreateForm(TServerPropertiesForm, ServerPropertiesForm);
  Application.CreateForm(TPassManForm, PassManForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
end.

