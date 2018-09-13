unit ServerPropertiesFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, LNet, LNetComponents, MQTTConsts, MQTTServer;

type

  { TServerPropertiesForm }

  TServerPropertiesForm = class(TForm)
    cbAllowNullClientIDs: TCheckBox;
    cbStrictClientIDValidation: TCheckBox;
    cbMaximumQoS: TComboBox;
    edAddress: TEdit;
    lbAddress: TLabel;
    lbMaxSessionAge: TLabel;
    lbResendPacketTimeout: TLabel;
    lbMaxResendAttempts: TLabel;
    lbMaxSubscriptionAge: TLabel;
    lbPort: TLabel;
    lbMaximumQoS: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    cbEnabled: TCheckBox;
    cbAuthentication: TCheckBox;
    seMaxSessionAge: TSpinEdit;
    sePort: TSpinEdit;
    seResendPacketTimeout: TSpinEdit;
    seMaxResendAttempts: TSpinEdit;
    seMaxSubscriptionAge: TSpinEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ServerPropertiesForm: TServerPropertiesForm;

function ServerPropertiesDlg(AServer: TMQTTServer; ATCP: TLTCPComponent): Boolean;

implementation

{$R *.lfm}

function ServerPropertiesDlg(AServer: TMQTTServer; ATCP: TLTCPComponent): Boolean;
begin
  ServerPropertiesForm.cbEnabled.Checked := AServer.Enabled;
  ServerPropertiesForm.cbAuthentication.Checked := AServer.RequireAuthentication;
  ServerPropertiesForm.cbAllowNullClientIDs.Checked := AServer.AllowNullClientIDs;
  ServerPropertiesForm.cbMaximumQOS.ItemIndex := ord(AServer.MaximumQOS);
  ServerPropertiesForm.cbStrictClientIDValidation.Checked := AServer.StrictClientIDValidation;
  ServerPropertiesForm.edAddress.Text := ATCP.Host;
  ServerPropertiesForm.sePort.Value := ATCP.Port;
  ServerPropertiesForm.seResendPacketTimeout.Value := AServer.ResendPacketTimeout;
  ServerPropertiesForm.seMaxResendAttempts.Value := AServer.MaxResendAttempts;
  ServerPropertiesForm.seMaxSubscriptionAge.Value := AServer.MaxSubscriptionAge;
  ServerPropertiesForm.seMaxSessionAge.Value := AServer.MaxSessionAge;

  ServerPropertiesForm.ActiveControl := ServerPropertiesForm.edAddress;

  Result := ServerPropertiesForm.ShowModal = mrOK;
  if Result then
    begin
      AServer.MaximumQOS := TMQTTQOSType(ServerPropertiesForm.cbMaximumQOS.ItemIndex);
      AServer.RequireAuthentication := ServerPropertiesForm.cbAuthentication.Checked;
      AServer.Enabled := ServerPropertiesForm.cbEnabled.Checked;
      AServer.AllowNullClientIDs := ServerPropertiesForm.cbAllowNullClientIDs.Checked;
      AServer.StrictClientIDValidation := ServerPropertiesForm.cbStrictClientIDValidation.Checked;
      AServer.ResendPacketTimeout := ServerPropertiesForm.seResendPacketTimeout.Value;
      AServer.MaxResendAttempts := ServerPropertiesForm.seMaxResendAttempts.Value;
      AServer.MaxSubscriptionAge := ServerPropertiesForm.seMaxSubscriptionAge.Value;
      AServer.MaxSessionAge := ServerPropertiesForm.seMaxSessionAge.Value;
      ATCP.Host := ServerPropertiesForm.edAddress.Text;
      ATCP.Port := ServerPropertiesForm.sePort.Value;
    end;
end;

end.

