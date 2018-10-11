unit connectfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, EditBtn, ButtonPanel, LNetSSL;

type

  { TMQTTConnectDlg }

  TMQTTConnectDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    cbClean: TCheckBox;
    cbWillMessageEnabled: TCheckBox;
    cbQOS: TComboBox;
    cbRetain: TCheckBox;
    cbUseSSL: TCheckBox;
    cbDefaultPort: TCheckBox;
    SSLVersionCombo: TComboBox;
    edClientID: TEdit;
    edServer: TEdit;
    edPrivateKeyPassword: TEdit;
    edKeepAlive: TSpinEdit;
    edMessage: TEdit;
    edPassword: TEdit;
    edTopic: TEdit;
    edUsername: TEdit;
    edPrivateKeyFile: TFileNameEdit;
    edCertificateFile: TFileNameEdit;
    gbWillMessage: TGroupBox;
    SSLSettingsGroup: TGroupBox;
    gmSession: TGroupBox;
    lbPrivateKeyFile: TLabel;
    lbCertificateFile: TLabel;
    lbSSLVersionCombo: TLabel;
    lbClientID: TLabel;
    lbPort: TLabel;
    lbKeepAlive: TLabel;
    lbMessage: TLabel;
    lbPassword: TLabel;
    lbQOS: TLabel;
    lbServer: TLabel;
    lbPrivateKeyPassword: TLabel;
    lbTopic: TLabel;
    lbUsername: TLabel;
    sePort: TSpinEdit;
    procedure cbDefaultPortChange(Sender: TObject);
    procedure cbWillMessageEnabledChange(Sender: TObject);
    procedure cbUseSSLChange(Sender: TObject);
  private
    { private declarations }
  public
    function GetSSLMethod: TLSSLMethod;
  end;

var
  MQTTConnectDlg: TMQTTConnectDlg;

implementation

{$R *.lfm}

{ TMQTTConnectDlg }

procedure TMQTTConnectDlg.cbDefaultPortChange(Sender: TObject);
begin
  if cbDefaultPort.Checked then
    begin
      if cbUseSSL.Checked then
        sePort.Value := 8883
      else
        sePort.Value := 1883;
      sePort.Enabled := False;
    end
  else
    sePort.Enabled := True;
end;

procedure TMQTTConnectDlg.cbWillMessageEnabledChange(Sender: TObject);
begin
  gbWillMessage.Enabled := cbWillMessageEnabled.Checked;
end;

procedure TMQTTConnectDlg.cbUseSSLChange(Sender: TObject);
begin
  SSLSettingsGroup.Enabled := cbUseSSL.Checked;
  if cbDefaultPort.Checked then
    begin
      if cbUseSSL.Checked then
        sePort.Value := 8883
      else
        sePort.Value := 1883;
    end
end;

function TMQTTConnectDlg.GetSSLMethod: TLSSLMethod;
begin
  case SSLVersionCombo.ItemIndex of
    0: Result := msSSLv2;
    1: Result := msSSLv3;
    2: Result := msSSLv2or3;
    3: Result := msTLSv1;
  end;
end;

end.

