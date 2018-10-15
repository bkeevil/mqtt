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
    ButtonPanel: TButtonPanel;
    cbAllowNullClientIDs: TCheckBox;
    cbStoreQOS0Messages: TCheckBox;
    cbAuthentication: TCheckBox;
    cbEnabled: TCheckBox;
    cbListenTLS: TCheckBox;
    cbListenUnencrypted: TCheckBox;
    cbMaximumQoS: TComboBox;
    cbStrictClientIDValidation: TCheckBox;
    edAddress: TEdit;
    edCertificateFile: TFileNameEdit;
    edPrivateKeyFile: TFileNameEdit;
    edPasswordFile: TFileNameEdit;
    edPrivateKeyPassword: TEdit;
    lbPasswordFile: TLabel;
    MQTTSettingsGroup: TGroupBox;
    TCPSettingsGroupBox: TGroupBox;
    lbAddress: TLabel;
    lbCertificateFile: TLabel;
    lbMaximumQoS: TLabel;
    lbMaxSessionAge: TLabel;
    lbMaxSubscriptionAge: TLabel;
    lbPort: TLabel;
    lbPrivateKeyFile: TLabel;
    lbPrivateKeyPassword: TLabel;
    lbTLSPort: TLabel;
    lbTLSVersionCombo: TLabel;
    seMaxSessionAge: TSpinEdit;
    seMaxSubscriptionAge: TSpinEdit;
    sePort: TSpinEdit;
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

function ServerPropertiesDlg(AServer: TMQTTServer; var StartNormalListener, StartTLSListener: Boolean; var PasswordFilename: String; ATCP, ASSLTCP: TLTCPComponent; ASSL: TLSSLSessionComponent): Boolean;

implementation

{$R *.lfm}

function ServerPropertiesDlg(AServer: TMQTTServer; var StartNormalListener, StartTLSListener: Boolean; var PasswordFilename: String; ATCP, ASSLTCP: TLTCPComponent; ASSL: TLSSLSessionComponent): Boolean;
begin
  ServerPropertiesForm.cbEnabled.Checked := AServer.Enabled;
  ServerPropertiesForm.cbAuthentication.Checked := AServer.RequireAuthentication;
  ServerPropertiesForm.cbAllowNullClientIDs.Checked := AServer.AllowNullClientIDs;
  ServerPropertiesForm.cbStoreQOS0Messages.Checked := AServer.StoreOfflineQoS0Messages;
  ServerPropertiesForm.cbMaximumQOS.ItemIndex := ord(AServer.MaximumQOS);
  ServerPropertiesForm.cbStrictClientIDValidation.Checked := AServer.StrictClientIDValidation;
  ServerPropertiesForm.edAddress.Text := ATCP.Host;
  ServerPropertiesForm.sePort.Value := ATCP.Port;
  ServerPropertiesForm.seMaxSubscriptionAge.Value := AServer.MaxSubscriptionAge;
  ServerPropertiesForm.seMaxSessionAge.Value := AServer.MaxSessionAge;
  ServerPropertiesForm.cbListenTLS.Checked := StartTLSListener;
  ServerPropertiesForm.edCertificateFile.Filename := ASSL.CAFile;
  ServerPropertiesForm.edPrivateKeyFile.Filename := ASSL.KeyFile;
  ServerPropertiesForm.SetSSLMethod(ASSL.Method);
  ServerPropertiesForm.edPrivateKeyPassword.Text := ASSL.Password;
  ServerPropertiesForm.seTLSPort.Value := ASSLTCP.Port;
  ServerPropertiesForm.ActiveControl := ServerPropertiesForm.edAddress;
  ServerPropertiesForm.edPasswordFile.Filename := PasswordFilename;
  Result := ServerPropertiesForm.ShowModal = mrOK;

  if Result then
    begin
      AServer.MaximumQOS := TMQTTQOSType(ServerPropertiesForm.cbMaximumQOS.ItemIndex);
      AServer.RequireAuthentication := ServerPropertiesForm.cbAuthentication.Checked;
      AServer.Enabled := ServerPropertiesForm.cbEnabled.Checked;
      AServer.AllowNullClientIDs := ServerPropertiesForm.cbAllowNullClientIDs.Checked;
      AServer.StoreOfflineQoS0Messages := ServerPropertiesForm.cbStoreQOS0Messages.Checked;
      AServer.StrictClientIDValidation := ServerPropertiesForm.cbStrictClientIDValidation.Checked;
      AServer.MaxSubscriptionAge := ServerPropertiesForm.seMaxSubscriptionAge.Value;
      AServer.MaxSessionAge := ServerPropertiesForm.seMaxSessionAge.Value;
      ATCP.Host := ServerPropertiesForm.edAddress.Text;
      ATCP.Port := ServerPropertiesForm.sePort.Value;
      StartTLSListener := ServerPropertiesForm.cbListenTLS.Checked;
      PasswordFilename := ServerPropertiesForm.edPasswordFile.Filename;
      ASSL.CAFile := ServerPropertiesForm.edCertificateFile.Filename;
      ASSL.KeyFile := ServerPropertiesForm.edPrivateKeyFile.Filename;
      ASSL.Method := ServerPropertiesForm.GetSSLMethod;
      ASSL.Password := ServerPropertiesForm.edPrivateKeyPassword.Text;
      ASSLTCP.Port := ServerPropertiesForm.seTLSPort.Value;
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

