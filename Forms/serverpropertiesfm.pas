unit ServerPropertiesFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, EditBtn, ButtonPanel, LNet, LNetComponents, LNetSSL,
  MQTTConsts, MQTTServer;

type

  { TServerPropertiesForm }

  TServerPropertiesForm = class(TForm)
    PasswordManagerBtn: TButton;
    ButtonPanel: TButtonPanel;
    cbAllowNullClientIDs: TCheckBox;
    cbAuthentication: TCheckBox;
    cbEnabled: TCheckBox;
    cbListenTLS: TCheckBox;
    cbListenUnencrypted: TCheckBox;
    cbMaximumQoS: TComboBox;
    cbStrictClientIDValidation: TCheckBox;
    edAddress: TEdit;
    edCertificateFile: TFileNameEdit;
    edPrivateKeyFile: TFileNameEdit;
    edPrivateKeyPassword: TEdit;
    MQTTSettingsGroup: TGroupBox;
    TCPSettingsGroupBox: TGroupBox;
    lbAddress: TLabel;
    lbCertificateFile: TLabel;
    lbMaximumQoS: TLabel;
    lbMaxResendAttempts: TLabel;
    lbMaxSessionAge: TLabel;
    lbMaxSubscriptionAge: TLabel;
    lbPort: TLabel;
    lbResendPacketTimeout: TLabel;
    lbPrivateKeyFile: TLabel;
    lbPrivateKeyPassword: TLabel;
    lbTLSPort: TLabel;
    lbTLSVersionCombo: TLabel;
    seMaxResendAttempts: TSpinEdit;
    seMaxSessionAge: TSpinEdit;
    seMaxSubscriptionAge: TSpinEdit;
    sePort: TSpinEdit;
    seResendPacketTimeout: TSpinEdit;
    seTLSPort: TSpinEdit;
    TLSSettingsGroup: TGroupBox;
    TLSVersionCombo: TComboBox;
    procedure cbListenTLSChange(Sender: TObject);
    procedure cbListenUnencryptedChange(Sender: TObject);
  private
    { private declarations }
  public
    function GetSSLMethod: TLSSLMethod;
    procedure SetSSLMethod(const Value: TLSSLMethod);
  end;

var
  ServerPropertiesForm: TServerPropertiesForm;

function ServerPropertiesDlg(AServer: TMQTTServer; var StartNormalListener, StartSSLListener: Boolean; ATCP, ASSLTCP: TLTCPComponent; ASSL: TLSSLSessionComponent): Boolean;

implementation

{$R *.lfm}

function ServerPropertiesDlg(AServer: TMQTTServer; var StartNormalListener, StartSSLListener: Boolean; ATCP, ASSLTCP: TLTCPComponent; ASSL: TLSSLSessionComponent): Boolean;
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

  ServerPropertiesForm.cbListenSSL.Checked := StartSSLListener;
  ServerPropertiesForm.edCertificateFile.Filename := ASSL.CAFile;
  ServerPropertiesForm.edPrivateKeyFile.Filename := ASSL.KeyFile;
  ServerPropertiesForm.SetSSLMethod(ASSL.Method);
  ServerPropertiesForm.edPrivateKeyPassword.Text := ASSL.Password;
  ServerPropertiesForm.seSSLPort.Value := ASSLTCP.Port;

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
      StartSSLListener := ServerPropertiesForm.cbListenSSL.Checked;
      ASSL.CAFile := ServerPropertiesForm.edCertificateFile.Filename;
      ASSL.KeyFile := ServerPropertiesForm.edPrivateKeyFile.Filename;
      ASSL.Method := ServerPropertiesForm.GetSSLMethod;
      ASSL.Password := ServerPropertiesForm.edPrivateKeyPassword.Text;
      ASSLTCP.Port := ServerPropertiesForm.seSSLPort.Value;
    end;
end;

{ TServerPropertiesForm }

procedure TServerPropertiesForm.cbListenTLSChange(Sender: TObject);
begin
  TLSSettingsGroup.Enabled := cbListenTLS.Checked;
  seTLSPort.Enabled := cbListenTLS.Checked;
end;

procedure TServerPropertiesForm.cbListenUnencryptedChange(Sender: TObject);
begin
  sePort.Enabled := cbListenUnencrypted.Checked;
end;

function TServerPropertiesForm.GetSSLMethod: TLSSLMethod;
begin
  case TLSVersionCombo.ItemIndex of
    0: Result := msSSLv2;
    1: Result := msSSLv3;
    2: Result := msSSLv2or3;
    3: Result := msTLSv1;
  end;
end;

procedure TServerPropertiesForm.SetSSLMethod(const Value: TLSSLMethod);
begin
  case Value of
    msSSLv2: TLSVersionCombo.ItemIndex := 0;
    msSSLv3: TLSVersionCombo.ItemIndex := 1;
    msSSLv2or3: TLSVersionCombo.ItemIndex := 2;
    msTLSv1: TLSVersionCombo.ItemIndex := 3;
  end;
end;

end.

