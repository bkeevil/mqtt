unit ServerFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, Menus, ExtCtrls, StdCtrls, ActnList, Buffers, Logging, LNet,
  LNetComponents, MQTTConsts, MQTTSubscriptions, MQTTServer, MQTTMessages;

const
  LISTEN_RETRY_DELAY    = 15000;
  MQTT_DEFAULT_CONFIG_FILENAME1           = '/etc/mqtt/mqtt.ini';
  MQTT_DEFAULT_CONFIG_FILENAME2           = 'mqttserver.ini';

type
  TDebugMessage = record
    MessageType: TLogMessageType;
    Module: String;
    Message: String;
  end;
  PDebugMessage = ^TDebugMessage;

  { TServerForm }

  TServerForm = class(TForm)
    CBEnabled: TCheckBox;
    cbEnableDebugMessages: TCheckBox;
    CBFiltered: TCheckBox;
    ClearBtn: TButton;
    ConnectionsGrid: TStringGrid;
    ConnectionsTab: TTabSheet;
    FilterText: TEdit;
    LogGrid: TStringGrid;
    LogToolbarPanel: TPanel;
    PageControl: TPageControl;
    RetainedMessagesGrid: TStringGrid;
    RetainedMessagesTab: TTabSheet;
    ServerPropertiesAction: TAction;
    RestartSeverAction: TAction;
    PasswordManagerAction: TAction;
    ExitAction: TAction;
    SaveConfigurationAction: TAction;
    LoadConfigurationAction: TAction;
    ActionList: TActionList;
    PasswordManagerItm: TMenuItem;
    RMDatastore: TMQTTRetainedMessagesDatastore;
    RestartServerItm: TMenuItem;
    SessionsGrid: TStringGrid;
    SessionsTab: TTabSheet;
    SubscriptionsGrid: TStringGrid;
    SubscriptionsTab: TTabSheet;
    TabSheet1: TTabSheet;
    TLS: TLSSLSessionComponent;
    TLSTCP: TLTCPComponent;
    RefreshRetainedMessagesItm: TMenuItem;
    RetainedMessagesGridMenu: TPopupMenu;
    TCP: TLTCPComponent;
    MainMenu: TMainMenu;
    LoadConfigurationItm: TMenuItem;
    MenuDividerItem1: TMenuItem;
    ConnectionsGridMenu: TPopupMenu;
    RefreshSessionsItm: TMenuItem;
    SessionsGridMenu: TPopupMenu;
    Server: TMQTTServer;
    RefreshSubscriptionsItm: TMenuItem;
    SubscriptionsGridMenu: TPopupMenu;
    RefreshConnectionsItm: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveConfigurationItm: TMenuItem;
    SaveDialog: TSaveDialog;
    ServerMenu: TMenuItem;
    PropertiesItm: TMenuItem;
    ExitItm: TMenuItem;
    ListenTimer: TTimer;
    ToolBar: TToolBar;
    ToolbarButtons: TImageList;
    LoadConfigurationBtn: TToolButton;
    SaveConfigurationBtn: TToolButton;
    ToolbarDivider1: TToolButton;
    ExitBtn: TToolButton;
    ToolbarDivider2: TToolButton;
    ServerPropertiesBtn: TToolButton;
    RestartServerBtn: TToolButton;
    PassswordManagerBtn: TToolButton;
    procedure CBEnabledChange(Sender: TObject);
    procedure cbEnableDebugMessagesChange(Sender: TObject);
    procedure CBFilteredChange(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ExitItmClick(Sender: TObject);
    procedure FilterTextExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListenTimerTimer(Sender: TObject);
    procedure LoadConfigurationItmClick(Sender: TObject);
    procedure PasswordManagerActionExecute(Sender: TObject);
    procedure RestartServerItmClick(Sender: TObject);
    procedure TLSTLSAccept(aSocket: TLSocket);
    procedure PropertiesItmClick(Sender: TObject);
    procedure RefreshConnectionsItmClick(Sender: TObject);
    procedure RefreshRetainedMessagesItmClick(Sender: TObject);
    procedure RefreshSessionsItmClick(Sender: TObject);
    procedure RefreshSubscriptionsItmClick(Sender: TObject);
    procedure SaveConfigurationItmClick(Sender: TObject);
    procedure ServerAccepted(AConnection: TMQTTServerConnection);
    procedure ServerDisconnect(AConnection: TMQTTServerConnection);
    procedure ServerDisconnected(AConnection: TMQTTServerConnection);
    procedure ServerError(AConnection: TMQTTServerConnection; ErrCode: Word; ErrMsg: String);
    procedure ServerSendData(AConnection: TMQTTServerConnection);
    procedure TCPAccept(aSocket: TLSocket);
    procedure TCPCanSend(aSocket: TLSocket);
    procedure TCPDisconnect(aSocket: TLSocket);
    procedure TCPError(const msg: string; aSocket: TLSocket);
    procedure TCPReceive(aSocket: TLSocket);
  private
    FConfigFilename : String;
    FListener       : TLogListener;
    FLogFile        : TLogFileListener;
    FCRTListener    : TLogCRTListener;
    //
    FRecords        : TList;
    procedure ClearRecords;
    procedure DisplayHelp;
    procedure HandleMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
    procedure LoadCommandLineOptions;
    function PassesFilter(Filter: String; Rec: PDebugMessage): Boolean;
    procedure Filter(Filter: String);

    function CheckConfigWriteAccess(Filename: String): Boolean;
    procedure LoadConfiguration(Filename: String);
    procedure RefreshAll;
    procedure SaveConfiguration(Filename: String);
  public
    Log: TLogDispatcher;
    StartNormalListener: Boolean;
    StartTLSListener: Boolean;
  end;

var
  ServerForm: TServerForm;
  C: Integer = 0;

implementation

{$R *.lfm}

uses
  MQTTPackets, MQTTPacketDefs, IniFiles, Crypto, Base32, PassManFM, HelpFM, ServerPropertiesFM, LNetSSL, OpenSSL;

const
  SYSTEM_PASSWORD = 'st5F2wrhXAaFVE9jzgKw';

{ TLSSLSocketHelper }

type
  TLSSLSocketHelper = class helper for TLSSLSocket
    function GetSSLPointer: PSSL;
  end;

{ TLSSLSocketHelper }

function TLSSLSocketHelper.GetSSLPointer: PSSL;
begin
  Result := FSSL;
end;

{ TServerForm }

procedure TServerForm.FormCreate(Sender: TObject);
var
  S: String;
begin
  Log := TLogDispatcher.Create('ServerForm');
  Log.Filter := ALL_LOG_MESSAGE_TYPES;
  FCRTListener := TLogCRTListener.Create;
  FRecords := TList.Create;
  FListener := TLogListener.Create;
  FListener.OnMessage := @HandleMessage;
  StartNormalListener := True;
  StartTLSListener := False;

  S := Application.CheckOptions('l:c:ansti:p:k:q:h','log-file: title: config: authenticate null-clientid strict-clientid store-qos0'+
                                'disabled debug tls tls-only interface: port: tls-interface: tls-port: pkey: cert: pkey-password: '+
                                'keepalive: max-session-age: max-subs-age: max-qos: help');

  if S > '' then
    begin
      Log.Send(mtError,S);
      DisplayHelp;
    end;
  S := GetCurrentDir;
  Log.Send(mtDebug,'Current working directory is "%s"',[S]);
  if Application.HasOption('c','config') then
    FConfigFilename := Application.GetOptionValue('c','config')
  else
    if FileExists(MQTT_DEFAULT_CONFIG_FILENAME1) then
      FConfigFilename := MQTT_DEFAULT_CONFIG_FILENAME1
    else
      FConfigFilename := MQTT_DEFAULT_CONFIG_FILENAME2;

  FConfigFilename := ExpandFilename(FConfigFilename);
  Log.Send(mtDebug,'ConfigFilename="%s"',[FConfigFilename]);
  OpenDialog.InitialDir := GetCurrentDir;
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  OpenDialog.Filename := FConfigFilename;
  SaveDialog.Filename := FConfigFilename;
  if FileExists(FConfigFilename) then
    LoadConfiguration(FConfigFilename)
  else
    Log.Send(mtWarning,'Config file %s not found. Using default values.',[FConfigFilename]);
  LoadCommandLineOptions;
  if Assigned(FLogFile) then
    Log.Send(mtDebug,'LogFile=%s',[FLogFile.Filename]);
  Log.Send(mtDebug,'RequireAuthentication=%s',[BoolToStr(Server.RequireAuthentication,'True','False')]);
  Log.Send(mtDebug,'AllowNullClientIDs=%s',[BoolToStr(Server.AllowNullClientIDs,'True','False')]);
  Log.Send(mtDebug,'StrictClientIDValidation=%s',[BoolToStr(Server.StrictClientIDValidation,'true','false')]);
  Log.Send(mtDebug,'StoreOfflineQoS0Messages=%s',[BoolToStr(Server.StoreOfflineQoS0Messages,'true','false')]);
  Log.Send(mtDebug,'KeepAlive=%d',[Server.KeepAlive]);
  Log.Send(mtDebug,'MaxSessionAge=%d',[Server.MaxSessionAge]);
  Log.Send(mtDebug,'MaxSubscriptionAge=%d',[Server.MaxSubscriptionAge]);
  Log.Send(mtDebug,'MaximumQoS=%s',[getQOSTypeName(Server.MaximumQoS)]);
  Log.Send(mtDebug,'StartNormalListener=%s',[BoolToStr(StartNormalListener)]);
  Log.Send(mtDebug,'StartTLSListener=%s',[BoolToStr(StartTLSListener)]);
  Log.Send(mtDebug,'ListenInterface=%s',[TCP.Host]);
  Log.Send(mtDebug,'ListenPort=%d',[TCP.Port]);
  Log.Send(mtDebug,'TLSInterface=%s',[TLSTCP.Host]);
  Log.Send(mtDebug,'TLSPort=%d',[TLSTCP.Port]);
  Log.Send(mtDebug,'PKeyFilename=%s',[TLS.KeyFile]);
  Log.Send(mtDebug,'CertFilename=%s',[TLS.CAFile]);
  Log.Send(mtDebug,'PasswordSupplied=%s',[BoolToStr(TLS.Password > '')]);
  if not Server.Enabled then
    Log.Send(mtWarning,'The server is being started in a disabled state');
  SaveConfigurationItm.Enabled := CheckConfigWriteAccess(FConfigFilename);
  PageControl.ActivePage := ConnectionsTab;
end;

procedure TServerForm.FormDestroy(Sender: TObject);
begin
  if StartNormalListener then
    TCP.Disconnect;
  if StartTLSListener then
    TLSTCP.Disconnect;
  if SaveConfigurationItm.Enabled then
    try
      SaveConfiguration(FConfigFilename);
    except
    end;
  Log.Free;
  FListener.Destroy;
  FCRTListener.Free;
  if Assigned(FLogFile) then
    FLogFile.Free;
  ClearRecords;
  FRecords.Destroy;
end;

procedure TServerForm.ExitItmClick(Sender: TObject);
begin
  Close;
end;

procedure TServerForm.ListenTimerTimer(Sender: TObject);
var
  Retry: Boolean = False;
begin
  ListenTimer.Enabled := False;
  if Server.Enabled then
    begin
      if StartNormalListener then
        if (not (Assigned(TCP.Session) and TCP.Session.Active)) and TCP.Listen then
          begin
            Log.Send(mtInfo,'Server listening on port %d',[TCP.Port]);
            ConnectionsGrid.Enabled := True;
            SubscriptionsGrid.Enabled := True;
            SessionsGrid.Enabled := True;
            RetainedMessagesGrid.Enabled := True;
            RefreshAll;
          end
        else
          Retry := True;
      if StartTLSListener then
        if (not (Assigned(TLSTCP.Session) and TLSTCP.Session.Active)) and TLSTCP.Listen then
          begin
            Log.Send(mtInfo,'TLS Server listening on port %d',[TLSTCP.Port]);
            ConnectionsGrid.Enabled := True;
            SubscriptionsGrid.Enabled := True;
            SessionsGrid.Enabled := True;
            RetainedMessagesGrid.Enabled := True;
            RefreshAll;
          end
        else
          Retry := True;
      if Retry then
        begin
          ListenTimer.Interval := LISTEN_RETRY_DELAY;
          Log.Send(mtError,'Server could not enter listening state.  Trying again in %d seconds.',[ListenTimer.Interval div 1000]);
          ListenTimer.Enabled := True;
        end;
    end;
end;

procedure TServerForm.LoadCommandLineOptions;
var
  S: String;
  I: Integer;
begin
  // log-file
  if Application.HasOption('l','log-file') then
    begin
      S := Application.GetOptionValue('l','log-file');
      if Assigned(FLogFile) and (FLogFile.Filename <> S) then
        FLogFile.Free;
      if S > '' then
        FLogFile := TLogFileListener.Create(S,True);
    end;
  // title
  if Application.HasOption('title') then
    begin
      S := Application.GetOptionValue('title');
      if S > '' then
        begin
          Application.Title := S;
          Caption := S;
        end;
    end;
  // debug
  if Application.HasOption('debug') then
    begin
      FLogFile.TypeFilter := ALL_LOG_MESSAGE_TYPES;
      FCRTListener.TypeFilter := ALL_LOG_MESSAGE_TYPES;
      FListener.TypeFilter := ALL_LOG_MESSAGE_TYPES;
    end;
  // disabled
  if Application.HasOption('disabled') then
    Server.Enabled := False;
  // interface
  if Application.HasOption('i','interface') then
    TCP.Host := Application.GetOptionValue('i','interface');
  // port
  if Application.HasOption('p','port') then
      begin
        S := Application.GetOptionValue('p','port');
        if TryStrToInt(S,I) and (I > 80) and (I < 65535) then
          TCP.Port := I
        else
          Log.Send(mtWarning,'Invalid port number on command line');
      end;
  // tls-interface
  if Application.HasOption('tls-interface') then
    TLSTCP.Host := Application.GetOptionValue('tls-interface');
  // tls-port
  if Application.HasOption('tls-port') then
      begin
        S := Application.GetOptionValue('tls-port');
        if TryStrToInt(S,I) and (I > 80) and (I < 65535) then
          TCP.Port := I
        else
          Log.Send(mtWarning,'Invalid TLS ort number on command line');
      end;
  // tls
  if Application.HasOption('t','tls') then
    StartTLSListener := True;
  // tls-only
  if Application.HasOption('tls-only') then
    StartNormalListener := False;
  // pkey
  if Application.HasOption('pkey') then
    begin
      S := Application.GetOptionValue('pkey');
      if (S > '') and FileExists(S) then
        TLS.KeyFile := S
      else
        begin
          StartTLSListener := False;
          Log.Send(mtWarning,'Private key file %s not found',[TLS.KeyFile]);
        end;
    end;
  // cert
  if Application.HasOption('cert') then
    begin
      S := Application.GetOptionValue('cert');
      if (S > '') and FileExists(S) then
        TLS.CAFile := S
      else
        begin
          StartTLSListener := False;
          Log.Send(mtWarning,'TLS Certificate file %s not found',[TLS.CAFile]);
        end;
    end;
  // pkey-password
  if Application.HasOption('pkey-password') then
    TLS.Password := Application.GetOptionValue('pkey-password');
  // keepalive
  if Application.HasOption('k','keepalive') then
    begin
      S := Application.GetOptionValue('k','keepalive');
      if TryStrToInt(S,I) then
        Server.KeepAlive := I
      else
        Log.Send(mtWarning,'keep-alive command line parameter not an integer');
    end;
  // authenticate
  if Application.HasOption('a','authenticate') then
    Server.RequireAuthentication := True;
  // null-clientid
  if Application.HasOption('n','null-clientID') then
    Server.AllowNullClientIDs := True;
  // strict-clientid
  if Application.HasOption('s','strict-clientid') then
    Server.StrictClientIDValidation := True;
  // store-qos0
  if Application.HasOption('store-qos0') then
    Server.StoreOfflineQoS0Messages := True;
  // max-session-age
  if Application.HasOption('max-session-age') then
    begin
      S := Application.GetOptionValue('max-session-age');
      if TryStrToInt(S,I) then
        Server.MaxSessionAge := I
      else
        Log.Send(mtWarning,'max-session-age command line parameter not an integer');
    end;
  // max-subs-age
  if Application.HasOption('max-subs-age') then
    begin
      S := Application.GetOptionValue('max-subs-age');
      if TryStrToInt(S,I) then
        Server.MaxSubscriptionAge := I
      else
        Log.Send(mtWarning,'max-subs-age command line parameter not an integer');
    end;
  // max-qos
  if Application.HasOption('max-qos') then
    begin
      S := Application.GetOptionValue('max-qos');
      if TryStrToInt(S,I) and (I >= 0) and (I < 3) then
        Server.MaximumQoS := TMQTTQOSType(I)
      else
        Log.Send(mtWarning,'max-qos command line parameter not an integer');
    end;
  // help
  if Application.HasOption('h','help') then
    DisplayHelp;
end;

procedure TServerForm.DisplayHelp;
begin
  if Application.ConsoleApplication then
    begin
      writeln('mqttserver Version 1.1 Useage:');
      writeln;
      writeln('  mqttserver -l <filename> --title <title> -c <filename> -i <IPAddress> -p <port>');
      writeln('  --tls-interface <IPAddress> -tls-port <port> --pkey <filename> --cert <filename>');
      writeln('  --pkey-password <password> --tls --keepalive <integer> --debug');
      writeln;
      writeln('  -l --log-file        Specify a log filename to use');
      writeln('     --title           Specify an alternate application title');
      writeln('  -c --config          Sets the default configuration file.  Default is');
      writeln('                       /etc/mqtt/mqttserver.ini or mqttserver.ini in the');
      writeln('                       current working directory if that is not found.');
      writeln('  -a --authenticate    When specified, clients are authenticated against the');
      writeln('                       password database.  Default is no authentication.');
      writeln('  -n --null-clientid   When no ClientID is provided by a client, generate a');
      writeln('                       unique one automatically.  Default rejects connection.');
      writeln('  -s --strict-clientid Validate Client IDs to ensure they contain only');
      writeln('                       letters and numbers.  Default accepts any character');
      writeln('     --store-QoS0      Ensures QoS0 messages dispatched to a disconnected session');
      writeln('                       are stored and resent when the connection is resumed ');
      writeln('                       rather than discarded.');
      writeln('     --debug           Add verbose debugging info to the log');
      writeln('     --disabled        Starts the server in disabled state.  Use to create a');
      writeln('                       config ini file using the GUI.');
      writeln('  -t --tls             Enable tls connections. Requires pkey and cert to be specified');
      writeln('     --tls-only        Only listen for TLS connections.');
      writeln('  -i --interface       Sets the IP address of the interface to listen on.');
      writeln('                       Default is all interfaces.');
      writeln('  -p --port            Sets the TCP port number to listen on.  Default 1883.');
      writeln('                       Applies to non-encrypted connections only.');
      writeln('     --tls-interface   Specify an interface to listen for TLS connections on.');
      writeln('     --tls-port        Sets the TCP port to accept TLS connections on. Default');
      writeln('                       is 8883.');
      writeln('     --pkey            Private key file to use for TLS connections in PEM format');
      writeln('     --cert            Public certificate file to use for TLS connections in PEM');
      writeln('                       format');
      writeln('     --pkey-password   Provide a password');
      writeln('  -k --keepalive       Specify an alternate keepalive value. Default 30 seconds');
      writeln('     --max-session-age Specify a maximum session age. Default is 1080 minutes');
      writeln('     --max-subs-age    Specify a maximum subscription age. Default is 1080 min');
      writeln('  -q --max-qos         Specify a maximum QoS level. Default is 2 (QoS2)');
      writeln('  -h --help            Displays this help message');
    end
  else
    begin
      HelpForm.ShowModal;
      Application.Terminate;
    end;
end;

function TServerForm.CheckConfigWriteAccess(Filename: String): Boolean;
var
  H: THandle;
begin
  if FileExists(Filename) then
    begin
      H := FileOpen(Filename,fmOpenReadWrite);
      Result := H <> feInvalidHandle;
      if Result then
        FileClose(H);
    end
  else
    begin
      H := FileCreate(Filename);
      Result := H <> feInvalidHandle;
      if Result then
        FileClose(H);
    end;
end;

procedure TServerForm.PropertiesItmClick(Sender: TObject);
begin
  if ServerPropertiesDlg(Server,StartNormalListener,StartTLSListener,TCP,TLSTCP,TLS) then
    begin
      SaveConfiguration(SaveDialog.Filename);
      RefreshAll;
    end;
end;

procedure TServerForm.LoadConfigurationItmClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    begin
      LoadConfiguration(OpenDialog.Filename);
      RefreshAll;
    end;
end;

procedure TServerForm.PasswordManagerActionExecute(Sender: TObject);
begin
  if not Assigned(PassManForm) then
    PassManForm := TPassManForm.Create(Application);
  PassManForm.Show;
end;

procedure TServerForm.RestartServerItmClick(Sender: TObject);
begin
  TCP.Disconnect;
  TLSTCP.Disconnect;
  ListenTimer.Enabled := True;
end;

procedure TServerForm.TLSTLSAccept(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
  PeerCertificate: PX509;
  P: PX509_NAME;
  S: String;
begin
  if (aSocket is TLSSLSocket) then
    begin
      Log.Send(mtInfo,'TLS connection accepted from %s on port %d',[aSocket.PeerAddress,aSocket.PeerPort]);
      PeerCertificate := SslGetPeerCertificate((aSocket as TLSSLSocket).GetSSLPointer);
      if Assigned(PeerCertificate) then
        begin
          P := X509GetSubjectName(PeerCertificate);
          if Assigned(P) then
            begin
              SetLength(S,255);
              X509NameOneline(P,S,255);
              Log.Send(mtInfo,'Subject Name: ' + Trim(S));
              S := '';
            end;
          P := X509GetSubjectName(PeerCertificate);
          if Assigned(P) then
            begin
              SetLength(S,255);
              X509NameOneline(P,S,255);
              Log.Send(mtInfo,'Issuer Name: ' + Trim(S));
            end;
        end;
      Conn := Server.StartConnection;
      aSocket.UserData := Conn;
      Conn.Socket := aSocket;
    end;
end;

procedure TServerForm.SaveConfigurationItmClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SaveConfiguration(SaveDialog.Filename);
end;

procedure TServerForm.LoadConfiguration(Filename: String);
var
  Ini: TInifile;
  S: String;
  I: Integer;
begin
  Ini := TInifile.Create(Filename,[ifoStripComments,ifoStripInvalid,ifoFormatSettingsActive]);
  try
    S := Ini.ReadString('General','LogFilename','');
    if S > '' then
      FLogFile := TLogFileListener.Create(S,True);
    if Ini.ReadBool('General','Debug',False) then
      begin
        FListener.TypeFilter := ALL_LOG_MESSAGE_TYPES;
        FCRTListener.TypeFilter := ALL_LOG_MESSAGE_TYPES;
        if Assigned(FLogFile) then
          FLogFile.TypeFilter := ALL_LOG_MESSAGE_TYPES;
        cbEnableDebugMessages.Checked := True;
      end
    else
      begin
        FListener.TypeFilter := DEFAULT_LOG_MESSAGE_TYPES;
        FCRTListener.TypeFilter := DEFAULT_LOG_MESSAGE_TYPES;
        if Assigned(FLogFile) then
          FLogFile.TypeFilter := DEFAULT_LOG_MESSAGE_TYPES;
        cbEnableDebugMessages.Checked := False;
      end;
    S := Ini.ReadString('General','Title','');
    if S > '' then
      begin
        Caption := S;
        Application.Title := S;
      end;
    Server.Enabled := Ini.ReadBool('MQTT','Enabled',True);
    Server.RequireAuthentication := Ini.ReadBool('MQTT','RequireAuthentication',False);
    Server.AllowNullClientIDs := Ini.ReadBool('MQTT','AllowNullClientIDs',False);
    Server.StrictClientIDValidation := Ini.ReadBool('MQTT','StrictClientIDValidation',False);
    Server.StoreOfflineQoS0Messages := Ini.ReadBool('MQTT','StoreOfflineQoS0Messages',False);
    Server.KeepAlive := Ini.ReadInteger('MQTT','KeepAlive',MQTT_DEFAULT_KEEPALIVE);
    Server.MaxSessionAge := Ini.ReadInteger('MQTT','MaxSessionAge',MQTT_DEFAULT_MAX_SESSION_AGE);
    Server.MaxSubscriptionAge := Ini.ReadInteger('MQTT','MaxSubscriptionAge',MQTT_DEFAULT_MAX_SUBSCRIPTION_AGE);
    //Server.MaxResendAttempts := Ini.ReadInteger('MQTT','MaxResendAttempts',MQTT_DEFAULT_MAX_RESEND_ATTEMPTS);
    //Server.ResendPacketTimeout := Ini.ReadInteger('MQTT','ResendPacketTimeout',MQTT_DEFAULT_RESEND_PACKET_TIMEOUT);
    I := Ini.ReadInteger('MQTT','MaximumQOS',2);

    StartNormalListener := Ini.ReadBool('Server','Listen',True);
    if I < 0 then
      I := 0;
    if I > 2 then
      I := 2;
    Server.MaximumQOS := TMQTTQOSType(I);
    TCP.Host := Ini.ReadString('Server','Interface','0.0.0.0');
    I := Ini.ReadInteger('Server','Port',1883);
        if I < 81 then
      I := 81;
    if I > 65535 then
      I := 65535;
    TCP.Port := I;

    StartTLSListener := Ini.ReadBool('TLS','Listen',False);
    TLSTCP.Host := Ini.ReadString('TLS','Interface',TCP.Host);
    I := Ini.ReadInteger('TLS','Port',8883);
        if I < 81 then
      I := 81;
    if I > 65535 then
      I := 65535;
    TLSTCP.Port := I;

    TLS.CAFile := Ini.ReadString('TLS','Certificate','');
    TLS.KeyFile := Ini.ReadString('TLS','Key','');
    S := Ini.ReadString('TLS','EncPassword','');
    if S > '' then
      TLS.Password := DecryptString(DecodeBase32Str(S),SYSTEM_PASSWORD)
    else
      TLS.Password := '';
  finally
    Ini.Free;
  end;
end;

procedure TServerForm.SaveConfiguration(Filename: String);
var
  Ini: TInifile;
  I: Integer;
begin
  Ini := TInifile.Create(Filename,[ifoStripComments,ifoStripInvalid,ifoFormatSettingsActive]);
  try
    if Assigned(FLogFile) then
      Ini.WriteString('General','LogFilename',FLogFile.Filename)
    else
      Ini.DeleteKey('General','LogFilename');
    Ini.WriteBool('General','Debug',cbEnableDebugMessages.Checked);
    Ini.WriteString('General','Title',Caption);
    Ini.WriteBool('MQTT','Enabled',Server.Enabled);
    Ini.WriteBool('MQTT','RequireAuthentication',Server.RequireAuthentication);
    Ini.WriteBool('MQTT','AllowNullClientIDs',Server.AllowNullClientIDs);
    Ini.WriteBool('MQTT','StrictClientIDValidation',Server.StrictClientIDValidation);
    Ini.WriteBool('MQTT','StoreOfflineQoS0Messages',Server.StoreOfflineQoS0Messages);
    Ini.WriteInteger('MQTT','KeepAlive',Server.KeepAlive);
    Ini.WriteInteger('MQTT','MaxSessionAge',Server.MaxSessionAge);
    Ini.WriteInteger('MQTT','MaxSubscriptionAge',Server.MaxSubscriptionAge);
    //Ini.WriteInteger('MQTT','MaxResendAttempts',Server.MaxResendAttempts);
    //Ini.WriteInteger('MQTT','ResendPacketTimeout',Server.ResendPacketTimeout);
    I := ord(Server.MaximumQOS);
    Ini.WriteInteger('MQTT','MaximumQOS',I);
    Ini.WriteBool('Server','Listen',StartNormalListener);
    Ini.WriteString('Server','BindAddress',TCP.Host);
    Ini.WriteInteger('Server','Port',TCP.Port);
    Ini.WriteBool('TLS','Listen',StartTLSListener);
    Ini.WriteString('TLS','BindAddress',TLSTCP.Host);
    Ini.WriteInteger('TLS','Port',TLSTCP.Port);
    Ini.WriteString('TLS','Certificate',TLS.CAFile);
    Ini.WriteString('TLS','Key',TLS.KeyFile);
    if TLS.Password > '' then
      Ini.WriteString('TLS','PasswordEnc',EncodeBase32Str(EncryptString(TLS.Password,SYSTEM_PASSWORD)))
    else
      Ini.DeleteKey('TLS','PasswordEnc');
  finally
    Ini.Free;
  end;
end;

procedure TServerForm.TCPAccept(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
begin
  Conn := Server.StartConnection;
  //Log.Send(mtDebug,'TCPAccept.ClassName=%s',[aSocket.ClassName]);
  Log.Send(mtInfo,'TCP connection accepted from %s on port %d',[aSocket.PeerAddress,aSocket.PeerPort]);
  aSocket.UserData := Conn;
  Conn.Socket := aSocket;
end;

procedure TServerForm.ServerAccepted(AConnection: TMQTTServerConnection);
var
  Username: String;
begin
  if AConnection.Socket is TLSocket then
    begin
      Username := AConnection.Username;
      Log.Send(mtInfo,'MQTT session started client=%s user=%s',[AConnection.Session.ClientID,Username]);
    end;
end;

procedure TServerForm.TCPDisconnect(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
begin
  if Assigned(ASocket) then
    begin
      Conn := TMQTTServerConnection(aSocket.UserData);
      aSocket.UserData := nil;
      if Assigned(Conn) then
        begin
          Conn.Socket := nil;
          if Assigned(Conn) then
              if Conn.State <> csDisconnected then
                Conn.Disconnected;
        end;
    end;
end;

procedure TServerForm.ServerDisconnect(AConnection: TMQTTServerConnection);
var
  Socket: TLSocket;
begin
  Socket := AConnection.Socket as TLSocket;
  if Assigned(Socket) and (Socket.ConnectionStatus = scConnected) then
    Socket.Disconnect(False);
  RefreshConnectionsItmClick(nil);
end;

procedure TServerForm.ServerDisconnected(AConnection: TMQTTServerConnection);
var
  Socket: TLSocket;
begin
  Socket := AConnection.Socket as TLSocket;
  if Assigned(Socket) and (Socket.ConnectionStatus = scConnected) then
    Socket.Disconnect(False);
  RefreshConnectionsItmClick(nil);
end;

procedure TServerForm.TCPError(const msg: string; aSocket: TLSocket);
begin
  Log.Send(mtError,Msg);
end;

procedure TServerForm.ServerError(AConnection: TMQTTServerConnection; ErrCode: Word; ErrMsg: String);
begin
  Log.Send(mtError,ErrMsg);
end;

procedure TServerForm.TCPReceive(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
  Data: Pointer;
  Size: Integer;
begin
  if Assigned(aSocket) and (aSocket.ConnectionStatus = scConnected) then
    begin
      Conn := TMQTTServerConnection(aSocket.UserData);
      if Assigned(Conn) and (Conn is TMQTTServerConnection) then
        begin
          GetMem(Data,32);
          try
            repeat
              Size := aSocket.Get(Data^,32);
              if Size > 0 then
                Conn.RecvBuffer.Write(Data,Size);
            until Size < 32;
            Conn.DataAvailable(Conn.RecvBuffer);
          finally
            FreeMem(Data,32);
          end;
        end;
    end;
end;

procedure TServerForm.ServerSendData(AConnection: TMQTTServerConnection);
begin
  TCPCanSend(AConnection.Socket as TLSocket);
end;

procedure TServerForm.TCPCanSend(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
  Data: Pointer;
  Sent,Size: Integer;
begin
  Conn := TMQTTServerConnection(aSocket.UserData);
  Size := Conn.SendBuffer.Size;
  GetMem(Data,Size);
  try
    Conn.SendBuffer.Peek(Data,Size);
    Sent := aSocket.Send(Data^,Size);
    Conn.SendBuffer.Read(Data,Sent);
  finally
    FreeMem(Data,Size);
  end;
end;

procedure TServerForm.RefreshAll;
begin
  RefreshConnectionsItmClick(nil);
  RefreshSessionsItmClick(nil);
  RefreshSubscriptionsItmClick(nil);
  RefreshRetainedMessagesItmClick(nil);
end;

function BoolToGridStr(B: Boolean): String;
begin
  if B then Result := '1' else Result := '0';
end;

procedure TServerForm.RefreshConnectionsItmClick(Sender: TObject);
var
  I: Integer;
  S: TMQTTServerConnection;
begin
  ConnectionsGrid.RowCount := Server.Connections.Count + 1;
  for I := 0 to Server.Connections.Count - 1 do
    begin
      S := Server.Connections[I];
      ConnectionsGrid.Objects[0,I+1] := S;
      ConnectionsGrid.Cells[0,I+1] := GetConnectionStateName(S.State);
      if (S.Socket is TLSocket) then
        ConnectionsGrid.Cells[1,I+1] := (S.Socket as TLSocket).PeerAddress
      else
        ConnectionsGrid.Cells[1,I+1] := '(N/A)';
      ConnectionsGrid.Cells[2,I+1]   := S.Username;
      ConnectionsGrid.Cells[3,I+1]   := S.WillMessage.DisplayText;
      ConnectionsGrid.Objects[3,I+1] := S.WillMessage;
    end;
end;

procedure TServerForm.RefreshSessionsItmClick(Sender: TObject);
var
  I: Integer;
  S: TMQTTSession;
begin
  SessionsGrid.RowCount := Server.Sessions.Count + 1;
  for I := 0 to Server.Sessions.Count - 1 do
    begin
      S := Server.Sessions[I];
      SessionsGrid.Objects[0,I+1] := S;
      SessionsGrid.Cells[0,I+1] := S.ClientID;
      SessionsGrid.Cells[1,I+1] := S.GetQueueStatusStr;
      SessionsGrid.Cells[2,I+1] := GetQOSTypeName(S.MaximumQOS);
      SessionsGrid.Cells[3,I+1] := IntToStr(S.Age);
      if S.CleanSession then
        SessionsGrid.Cells[4,I+1] := '1'
      else
        SessionsGrid.Cells[5,I+1] := '0';
    end;
end;

procedure TServerForm.RefreshSubscriptionsItmClick(Sender: TObject);
var
  I,J,X: Integer;
  C: TMQTTServerConnection;
  S: TMQTTSubscription;
begin
  J := 1;
  for I := 0 to Server.Connections.Count - 1 do
    begin
      C := Server.Connections[I];
      if Assigned(C) and Assigned(C.Session) then
        begin
          for X := 0 to C.Session.Subscriptions.Count - 1 do
            begin
              S := C.Session.Subscriptions[X];
              if J >= SubscriptionsGrid.RowCount then
                SubscriptionsGrid.RowCount     := SubscriptionsGrid.RowCount + 1;
              SubscriptionsGrid.Cells[0,J] := C.Session.ClientID;
              SubscriptionsGrid.Cells[1,J] := S.Filter;
              SubscriptionsGrid.Cells[2,J] := GetQOSTypeName(S.QOS);
              SubscriptionsGrid.Cells[3,J] := IntToStr(S.Age);
              inc(J);
            end;
        end;
    end;
  SubscriptionsGrid.RowCount := J;
end;

procedure TServerForm.RefreshRetainedMessagesItmClick(Sender: TObject);
var
  I: Integer;
  M: TMQTTMessage;
begin
  RetainedMessagesGrid.RowCount := Server.RetainedMessages.Count + 1;
  for I := 0 to Server.RetainedMessages.Count - 1 do
    begin
      M := Server.RetainedMessages[I];
      RetainedMessagesGrid.Objects[0,I+1] := M;
      RetainedMessagesGrid.Cells[0,I+1] := M.ClientID;
      RetainedMessagesGrid.Cells[1,I+1] := M.Topic;
      RetainedMessagesGrid.Cells[2,I+1] := M.Data;
      RetainedMessagesGrid.Cells[3,I+1] := GetQOSTypeName(M.QOS);
    end;
end;

procedure TServerForm.ClearRecords;
var
  I: Integer;
  P: PDebugMessage;
begin
  for I := 0 to FRecords.Count - 1 do
    begin
      P := FRecords[I];
      Dispose(P);
      dec(C);
    end;
  FRecords.Clear;
end;

procedure TServerForm.FilterTextExit(Sender: TObject);
begin
  if CBFiltered.Checked then
    begin
      LogGrid.RowCount := 1;
      Filter(FilterText.Text);
    end;
end;

procedure TServerForm.ClearBtnClick(Sender: TObject);
begin
  ClearRecords;
  LogGrid.RowCount := 1;
end;

procedure TServerForm.HandleMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
var
  P: PDebugMessage;
  LKind: String;
  LModule: String;
  LMessage: String;
begin
  New(P);
  inc(C);
  P^.MessageType := MessageType;
  if Assigned(Dispatcher) then
    P^.Module := Dispatcher.Name
  else
    P^.Module := '';
  P^.Message := Message;
  FRecords.Add(P);
  if (not CBFiltered.Checked) or PassesFilter(FilterText.Text,P) then
    begin
      LKind := MESSAGE_TYPE_STRINGS[P^.MessageType];
      LModule := P^.Module;
      LMessage := P^.Message;
      LogGrid.InsertColRow(False,LogGrid.RowCount);
      LogGrid.Cells[0,LogGrid.RowCount - 1] := LKind;
      LogGrid.Cells[1,LogGrid.RowCount - 1] := LModule;
      LogGrid.Cells[2,LogGrid.RowCount - 1] := LMessage;
      Show;
    end;
end;

procedure TServerForm.CBFilteredChange(Sender: TObject);
begin
  if CBFiltered.Checked then
    Filter(FilterText.Text)
  else
    Filter('');
end;

procedure TServerForm.CBEnabledChange(Sender: TObject);
begin
  FListener.Enabled := CBEnabled.Checked;
end;

procedure TServerForm.cbEnableDebugMessagesChange(Sender: TObject);
begin
  if cbEnableDebugMessages.Checked then
    begin
      FListener.TypeFilter := ALL_LOG_MESSAGE_TYPES;
      Server.Log.Filter := ALL_LOG_MESSAGE_TYPES;
    end
  else
    begin
      FListener.TypeFilter := DEFAULT_LOG_MESSAGE_TYPES;
      Server.Log.Filter := DEFAULT_LOG_MESSAGE_TYPES;
    end;
end;

function TServerForm.PassesFilter(Filter: String; Rec: PDebugMessage): Boolean;
begin
  if Filter = '' then
    Result := True
  else
    if Filter[1] = '-' then
      Result := (Pos(Copy(Filter, 2, Length(Filter) - 1), Rec^.Module) = 0) and
                (Pos(Copy(Filter, 2, Length(Filter) - 1), Rec^.Message) = 0)
    else
      Result := (Pos(Filter, Rec^.Module) > 0) or (Pos(Filter, Rec^.Module) > 0);
end;

procedure TServerForm.Filter(Filter: String);
var
  I: Integer;
  P: PDebugMessage;
begin
  LogGrid.RowCount := 1;
  for I := 0 to FRecords.Count - 1 do
    begin
      P := FRecords[I];
      if PassesFilter(Filter, P) then
        LogGrid.InsertRowWithValues(LogGrid.RowCount,[MESSAGE_TYPE_STRINGS[P^.MessageType],P^.Module,P^.Message]);
    end;
end;

end.

