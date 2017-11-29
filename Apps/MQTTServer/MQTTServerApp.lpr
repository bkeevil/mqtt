program MQTTServerApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, serverfm, ServerPropertiesFM,
  mqttregister, lnetvisual, helpfm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TServerForm, ServerForm);
  Application.CreateForm(TServerPropertiesForm, ServerPropertiesForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
end.

