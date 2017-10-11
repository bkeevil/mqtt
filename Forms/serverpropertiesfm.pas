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
    cbMaximumQoS: TComboBox;
    edAddress: TEdit;
    lbAddress: TLabel;
    lbPort: TLabel;
    lbMaximumQoS: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    cbEnabled: TCheckBox;
    cbAuthentication: TCheckBox;
    sePort: TSpinEdit;
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
  ServerPropertiesForm.edAddress.Text := ATCP.Host;
  ServerPropertiesForm.sePort.Value := ATCP.Port;

  ServerPropertiesForm.ActiveControl := ServerPropertiesForm.edAddress;
  Result := ServerPropertiesForm.ShowModal = mrOK;
  if Result then
    begin
      AServer.MaximumQOS := TMQTTQOSType(ServerPropertiesForm.cbMaximumQOS.ItemIndex);
      AServer.RequireAuthentication := ServerPropertiesForm.cbAuthentication.Checked;
      AServer.Enabled := ServerPropertiesForm.cbEnabled.Checked;
      AServer.AllowNullClientIDs := ServerPropertiesForm.cbAllowNullClientIDs.Checked;
      ATCP.Host := ServerPropertiesForm.edAddress.Text;
      ATCP.Port := ServerPropertiesForm.sePort.Value;
    end;
end;

end.

