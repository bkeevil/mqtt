program MQTTServerApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, serverfm, ServerPropertiesFM, passwordmanfm,
  mqttregister, lnetvisual, helpfm;

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

