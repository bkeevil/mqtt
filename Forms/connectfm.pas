unit connectfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin;

type

  { TConnectDlg }

  TConnectDlg = class(TForm)
    btnConnect: TButton;
    btnCancel: TButton;
    cbClean: TCheckBox;
    cbQOS: TComboBox;
    cbRetain: TCheckBox;
    cbEnabled: TCheckBox;
    edIPAddress: TEdit;
    edTopic: TEdit;
    edMessage: TEdit;
    edUsername: TEdit;
    edPassword: TEdit;
    edClientID: TEdit;
    gbWillMessage: TGroupBox;
    lbServerIP: TLabel;
    lbClientID2: TLabel;
    lbPassword: TLabel;
    lbUsername: TLabel;
    lbQOS: TLabel;
    lbTopic: TLabel;
    lbMessage: TLabel;
    lbKeepAlive: TLabel;
    lbClientID: TLabel;
    edKeepAlive: TSpinEdit;
    sePort: TSpinEdit;
  private
    { private declarations }
  public

  end;

var
  ConnectDlg: TConnectDlg;

implementation

{$R *.lfm}

end.

